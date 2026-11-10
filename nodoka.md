# nodoka 設定メモ

Windows のキーリマッパ [のどか](https://appletllc.com/web/nodoka-doc/MANUAL-ja.html) の設定 `user.nodoka` についてのメモ。

## 概要

ノートPC(Surface)の本体キーボードと、Bluetooth 接続の HHKB(JP配列)では、使いたいキー配置が少し違う。
以前は `hhkb.nodoka` / `nihongo.nodoka` / `common.nodoka` の3ファイルを、S-F2 で手動で切り替えていた。

別売のドライバ **kbdaddid.sys** を入れると、キーを押した時点で入力元のキーボードを判別できる。
これを使い、1ファイル(`user.nodoka`)で両方のキーボードを扱うようにした。

| 設定 | 本体KB (K1) | HHKB (K2) |
|---|---|---|
| 英数(Caps の位置) → Ctrl | する | しない(※) |
| Esc ⇔ 半角/全角 の入れ替え | する(全アプリ) | しない |
| 無変換 → Alt | 共通 | 共通 |
| 変換 → CapsLock | 共通 | 共通 |
| SansS、Emacs風キーバインドなど | 共通 | 共通 |

※ HHKB の英数は Windows へはそのまま英数として送られる。ただし、のどかの中では Ctrl 扱いになる(後述)。

## 導入手順

1. kbdaddid.sys を導入し、ライセンス認証しておく(Windows 11 のみ対応)。
2. `user.nodoka` をホームディレクトリ(`%USERPROFILE%`)にコピーする。
   - Windows では、シンボリックリンクではなくコピーで運用している。編集したら、こちらのリポジトリにもコピーし直すこと。
3. のどかの設定ダイアログで、`user.nodoka` を登録する(名前: `User`、シンボル: `-DNODOKA`)。
4. ログウィンドウを開き、本体KBと HHKB でキーを押す。ログにそれぞれ `K1-` / `K2-` が付いて表示されることを確認する。

## キーボードの識別(ExtraInfo の値の取り直し)

`def option UnitID = Kx ExtraInfo 0x....` で、キーボードを K1〜K7 に割り当てる。
割り当てのないキーボードは `K0-` になる。

値の確認方法は次の2つ。

- 設定を再読み込みすると、ログにデバイスごとの `kbdaddid ExtraInfo 0x........ (DeviceInstanceId mode)` が出る。
- 未登録のキーボードでキーを押すと、ログに次の行が出る。
  ```
  kbdaddid: unregistered hi=0x222. Add to .nodoka: def option UnitID = Kx ExtraInfo 0x2220000
  ```

**注意: HHKB は、この2つの値が一致しなかった。**
読み込み時の一覧に出た値(`0xBA780000`)を書いても K0 のままで、キー入力時に出た値(`0x2220000`)を書いたら K2 になった。
**値はキー入力時のログに出るものを信じる**こと。HHKB の電源を入れ直しても、値は変わらなかった。

本体KBは、Surface HID Keyboard として3つのデバイス(Target_SAM / HID_DEVICE_SYSTEM_VHF / ConvertedDevice)に分かれて見える。これらは全部 K1 にしてある。

識別方式(DeviceIdMode)は、既定の DeviceInstanceId mode を使っている。
再ペアリングなどで値が頻繁に変わるようなら、kbdaddid 側で HardwareId mode(同じ製品なら同じ値)に切り替えて、値を取り直す。

## ハマりどころ

### `mod` はキーボードごとに書き分けられない

`mod control += 英数` のようなモディファイヤの割り当ては、キー単位で全キーボードに効く。`K1-` などは書けない。
「本体KBの英数だけ Ctrl にする」は、次のように書くしかない。

```
mod control += 英数          # 全キーボードで、のどか内部の C- 扱いになる
key K1-*英数 = *LControl     # Windows へ LControl を送るのは本体KBだけ
```

この結果、HHKB の英数も、のどかの中では C- 扱いになる。
たとえば HHKB で英数+F を押すと、Emacs 風キーマップの `C-F` が反応する。HHKB の英数は普段使わないので許容している。

### `def subst` ではモディファイヤを作れない

最初は `def subst K1-*英数 = *LControl` を試した。
`def subst` は、キーマップより前にキーを置き換える仕組みで、K1- 付きでも書けて、置き換え自体は効く。
しかし、**のどかの「どのモディファイヤが押されているか」の判定は、置き換え前のキー(英数)で行われる**。

その結果、英数+A を押すと、A のイベントに C- が付かない。
のどかは「Ctrl は押されていないはず」と判断して、A を送る直前に LControl を離すイベントを送ってしまい、ただの `a` が入力された。
ログでは `* substitute` の直後に、`Gen Modifiers { => U-LeftControl }` が出ていた。

### `mod` と `key` は役割が違う

```
mod control += 英数
key *英数 = *LControl
```

この2行は同じことを2回書いているように見えるが、役割が違う。

- `mod`: のどかの中で、英数を C- として扱わせる。キーマップの `key C-F = ...` などが反応するようになる。
- `key`: Windows へ送るキーを LControl に変える。

`mod` だけだと、Windows には英数がそのまま届く。
`key` だけだと、のどかのキーマップの C- 規則が反応しない。

### 子キーマップの `&Default` は親の K1 規則を打ち消す

キーの検索は、子のキーマップから親へ順にたどる。
子に `key Esc = &Default` があると、親(Global)の `key K1-*Esc = *半角/全角` までたどり着かない。

以前は `MyEmacsEdit` と `Emacsen` にこの打ち消しがあった。そのため、メモ帳・Chrome・Windows 版 Emacs などでは、Esc と 半角/全角 が入れ替わっていなかった。
今は全アプリで入れ替えたいので、これらの行は削除した。

K を指定しない `key Esc` は、デフォルトモディファイヤで K0〜K3 が「どちらでも良い」扱いなので、全キーボードに一致する。
なお、K4〜K7 はデフォルトモディファイヤの一覧に載っていないので、K1〜K3 を使うのが無難。

### ウィンドウクラスで Emacs を判定する場合

`window Emacs /:Emacs$/` に一致するのは、Windows 版 Emacs(emacs.exe)だけ。
WSL2 の Emacs は WSLg 経由で表示されるので、ウィンドウクラスが違い、一致しない。

### 本体KB固有の事情

- 本体KBの変換・無変換の位置のキーは、IME オン/オフを送る。そのため、変換 → CapsLock は HHKB でしか効かない。
- 本体KBの無変換はスキャンコードが標準と違い、Windows が認識しない。そのため、無変換 → Alt も実質 HHKB 専用。
- 以前は HHKB 用に `mod alt -= LAlt` を書いていた。しかし `mod` は全キーボードに効き、本体KBの Alt まで失われるので削除した。HHKB では物理 Alt と無変換の両方が Alt になる。

## 動作確認に使えるコマンド(のどか 4.33 以降)

```
"C:\Program Files\nodoka\nodoka64.exe" -y -t User         # 登録名 User の設定に切り替えて再読み込み
"C:\Program Files\nodoka\nodoka64.exe" -y -d C:\path\nodoka.log   # ログをファイルにも出力
```

- `-t`: 起動済みののどかにも効く。
- `-d`: 起動済みののどかには効かない。最初に起動するときに付ける必要がある。

設定ファイルは UTF-8(BOM なし)/ CRLF で保存している。
のどか 4.33 は、Shift-JIS / UTF-8 / UTF-16 のどれでも読み込める。

## 参考

- [のどか マニュアル](https://appletllc.com/web/nodoka-doc/MANUAL-ja.html)
- [のどか カスタマイズ方法](https://appletllc.com/web/nodoka-doc/CUSTOMIZE-ja.html)
  - 「オプション 複数キーボード対応(kbdaddid.sys編)」
  - 「オプション 複数キーボード対応(従来版)」
  - 「代用定義」
  - 「モディファイヤキー割り当ての変更」
