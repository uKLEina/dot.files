(use-package ess-julia
  :mode (("\\.jl\\'" . ess-julia-mode))
  :config
  (use-package electric-operator)
  (apply #'electric-operator-add-rules-for-mode 'ess-julia-mode (electric-operator-get-rules-for-mode 'prog-mode))
  (electric-operator-add-rules-for-mode 'ess-julia-mode
                                        (cons "=" #'electric-operator-julia-mode-kwargs-=)
                                        (cons ";" "; ")

                                        ;; Subtype comparison
                                        (cons "<:" " <: ")

                                        ;; Cool! Unicode!
                                        (cons "÷" " ÷ ")
                                        (cons "≠" " ≠ ")
                                        (cons "≤" " ≤ ")
                                        (cons "≥" " ≥ ")

                                        ;; something about fractions
                                        (cons "//" " // ")
                                        (cons ".//" " .// ")
                                        (cons "//=" " //= ")

                                        ;; pipe
                                        (cons "|>" " |> ")

                                        (cons "*" " * ")
                                        (cons "/" " / ")
                                        (cons "%" " % ")
                                        (cons "&" " & ")

                                        ;; \ (escaped), for solving matrix multiplies
                                        (cons "\\" " \\ ")
                                        (cons "\\=" " \\= ")
                                        (cons ".\\" " .\\ ")

                                        ;; XOR
                                        (cons "$" " $ ")

                                        ;; Even more equal!
                                        (cons "===" " === ")
                                        (cons "!==" " !== ")

                                        ;; vector operations and assign-operators
                                        (cons ".^" " .^ ")
                                        (cons ".*" " .* ")
                                        (cons "./" " ./ ")
                                        (cons ".%" " .% ")
                                        (cons "<<" " << ")
                                        (cons ">>" " >> ")
                                        (cons ">>>" " >>> ")
                                        (cons ".<<" " .<< ")
                                        (cons ".>>" " .>> ")
                                        (cons ".>>>" " .>>> ")
                                        (cons ".+" " .+ ")
                                        (cons ".-" " .- ")
                                        (cons ".>" " .> ")
                                        (cons ".<" " .< ")
                                        (cons ".>=" " .>= ")
                                        (cons ".<=" " .<= ")
                                        (cons ".==" " .== ")
                                        (cons ".!=" " .!= ")
                                        (cons "^=" " ^= ")
                                        (cons "÷=" " ÷= ")
                                        (cons "%=" " %= ")
                                        (cons "|=" " |= ")
                                        (cons "&=" " &= ")
                                        (cons "$=" " $= ")
                                        (cons "<<=" " <<= ")
                                        (cons ">>=" " >>= ")
                                        (cons ">>>=" " >>>= ")
                                        (cons ".+=" " .+= ")
                                        (cons ".-=" " .-= ")
                                        (cons ".*=" " .*= ")
                                        (cons "./=" " ./= ")
                                        (cons ".//=" " .//= ")
                                        (cons ".\\=" " .\\= ")
                                        (cons ".^=" " .^= ")
                                        (cons ".÷=" " .÷= ")
                                        (cons ".%=" " .%= "))
  )
