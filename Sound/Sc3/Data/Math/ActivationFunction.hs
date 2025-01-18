-- | <https://en.wikipedia.org/wiki/Activation_function>
module Sound.Sc3.Data.Math.ActivationFunction where

{- | Identity

> Sound.Sc3.Plot.plot_fn_r1_ln [af_identity] (-6, 6)
-}
af_identity :: a -> a
af_identity = id

{- | Binary step

> Sound.Sc3.Plot.plot_fn_r1_ln [af_binary_step] (-4, 4)
-}
af_binary_step :: (Ord a, Num a) => a -> a
af_binary_step x = if x < 0 then 0 else 1

{- | Logistic, sigmoid, or soft step

> Sound.Sc3.Plot.plot_fn_r1_ln [af_logistic] (-6, 6)
-}
af_logistic :: Floating a => a -> a
af_logistic x = 1 / (1 + exp (-x))

{- | Hyperbolic tangent (tanh)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_tanh] (-6, 6)
-}
af_tanh :: Floating a => a -> a
af_tanh x = (exp x - exp (-x)) / (exp x + exp (-x))

{- | Rectified linear unit (ReLU)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_rectified_linear_unit] (-6, 6)
-}
af_rectified_linear_unit :: (Ord a, Num a) => a -> a
af_rectified_linear_unit x = max 0 x

{- | Gaussian Error Linear Unit (GELU)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_gaussian_error_linear_unit] (-3, 3)
-}
af_gaussian_error_linear_unit :: Floating a => a -> a
af_gaussian_error_linear_unit x = 0.5 * x * (1 + tanh (x * 0.7978845608 * (1 + 0.044715 * x * x)))

{- | Softplus

> Sound.Sc3.Plot.plot_fn_r1_ln [af_softplus] (-6, 6)
-}
af_softplus :: Floating a => a -> a
af_softplus x = log (1 + exp x)

{- | Exponential linear unit (ELU)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_exponential_linear_unit 1] (-6, 6)
-}
af_exponential_linear_unit :: (Ord a, Floating a) => a -> a -> a
af_exponential_linear_unit alpha x = if x <= 0 then alpha * (exp x - 1) else x

{- | Scaled exponential linear unit (SELU)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_scaled_exponential_linear_unit 1.0507 1.67326] (-6, 6)
-}
af_scaled_exponential_linear_unit :: (Ord a, Floating a) => a -> a -> a -> a
af_scaled_exponential_linear_unit lambda alpha x = lambda * af_exponential_linear_unit alpha x

{- | Parametric rectified linear unit (PReLU)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_parametric_rectified_linear_unit 0.5] (-6, 6)
-}
af_parametric_rectified_linear_unit :: (Ord a, Floating a) => a -> a -> a
af_parametric_rectified_linear_unit alpha x = if x < 0 then alpha * x else x

{- | Leaky rectified linear unit (Leaky ReLU)

> Sound.Sc3.Plot.plot_fn_r1_ln [af_leaky_rectified_linear_unit] (-6, 6)
-}
af_leaky_rectified_linear_unit :: (Ord a, Floating a) => a -> a
af_leaky_rectified_linear_unit = af_parametric_rectified_linear_unit 0.01

{- | Sigmoid linear unit (SiLU), Sigmoid shrinkage SiL, Swish-‍1

> Sound.Sc3.Plot.plot_fn_r1_ln [af_sigmoid_linear_unit] (-5, 5)
-}
af_sigmoid_linear_unit :: Floating a => a -> a
af_sigmoid_linear_unit x = x / (1 + exp (-x))

{- | Gaussian

> Sound.Sc3.Plot.plot_fn_r1_ln [af_gaussian] (-2.5, 2.5)
-}
af_gaussian :: Floating a => a -> a
af_gaussian x = exp (-(x * x))

{- | Softmax, <https://en.wikipedia.org/wiki/Softmax_function>

>>> let c = map (round . (* 100))
>>> c (softmax [1, 2, 3, 4, 1, 2, 3])
[2,6,17,47,2,6,17]

>>> c (softmax (map (* 0.1) [1, 2, 3, 4, 1, 2, 3]))
[12,14,15,17,12,14,15]
-}
softmax :: Floating n => [n] -> [n]
softmax l = let r = map exp l in map (/ (sum r)) r
