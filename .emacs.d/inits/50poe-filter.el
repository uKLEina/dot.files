(use-package generic-x)

(define-generic-mode 'poe-filter-mode
  ;; Comments
  '("#")
  ;; Blocks
  '("Show" "Hide")
  '(
    ;; Conditions
    ("\\(ItemLevel\\|DropLevel\\|Quality\\|Rarity\\|Class\\|BaseType\\|Sockets\\|LinkedSockets\\|SocketGroup\\|Height\\|Width\\|HasExplicitMod\\|StackSize\\|GemLevel\\|Identified\\|Corrupted\\|ElderItem\\|ShaperItem\\|ElderMap\\|ShapedMap\\|MapTier\\)" . 'font-lock-variable-name-face)
    ;; Actions
    ("\\(SetBorderColor\\|SetTextColor\\|SetBackgroundColor\\|SetFontSize\\|PlayAlertSound\\|PlayAlertSoundPositional\\|DisableDropSound\\|CustomAlertSound\\|MinimapIcon\\|PlayEffect\\)" . 'font-lock-variable-name-face)
    ;; Attributes
    ("\\(True\\|False\\)" . 'font-lock-constant-face)
    ;; Rarity
    ("\\(Unique\\|Rare\\|Magic\\|Normal\\)" . 'font-lock-constant-face)
    ;; Color
    ("\\(Red\\|Green\\|Blue\\|Brown\\|White\\|Yellow\\)" . 'font-lock-constant-face)
    ;; Shape
    ("\\(Circle\\|Diamond\\|Hexagon\\|Square\\|Star\\|Triangle\\)" . font-lock-constant-face)
    ;; Beam
    ("\\(Temp\\)" . font-lock-constant-face)
    ;; Base Type
    ("\\(Jewel\\|Amulets\\|Belt\\|Ring\\|Wands\\|Daggers\\|One Hand\\|Shields\\|Thrusting\\|Sceptre\\|Claws\\|Currency\\|Gems\\|Flask\\|Maps\\|Piece\\)" . font-lock-constant-face)
    ;; Item Size
    ("\\(Small\\|Medium\\|Large\\)" . font-lock-constant-face)
    ;; Flask Tier
    ("\\(Greater\\|Grand\\|Giant\\|Colossal\\|Sacred\\|Hallowed\\|Sanctified\\|Divine\\|Eternal\\)" . font-lock-constant-face)
    )
  '(".filter\\'")
  nil
  "Major mode for editing Path of Exile filter file.")
