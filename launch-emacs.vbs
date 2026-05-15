Set ws = CreateObject("WScript.Shell")
ws.Run "wsl -- zsh -lc ""cd ~ && emacs""", 0, False
