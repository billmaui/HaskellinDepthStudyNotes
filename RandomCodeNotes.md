# Random Study

One line change to verify that I understand commits. 

Another change after closing then opening vscode. 


# Hacking inside the Random docs

It's instructive to look at the source code for System.Random. The source is available on Hackage. It's also instructive to compare the code for version 1.2.0 and 1.2.1 because the latest version contains several important changes. 

Let's start with the global pseudo-random number generator. Compare the descriptions in the two versions:

## Random Version 1.2.0
>There is a single, implicit, global pseudo-random number generator of type 'StdGen', held in a global variable maintained by the 'IO' monad. It is initialised automatically in some system-dependent fashion. To get deterministic behaviour, use 'setStdGen'.

>Note that 'mkStdGen' also gives deterministic behaviour without requiring an 'IO' context.

## Random Version 1.2.1
>There is a single, implicit, global pseudo-random number generator of type 'StdGen', held in a global mutable variable that can be manipulated from within the 'IO' monad. It is also available as 'System.Random.Stateful.globalStdGen', therefore it is recommended to use the new "System.Random.Stateful" interface to explicitly operate on the global pseudo-random number generator.

>It is initialised with 'initStdGen', although it is possible to override its value with 'setStdGen'. All operations on the global pseudo-random number generator are thread safe, however in presence of concurrency they are naturally become non-deterministic. Moreover, relying on the global mutable
state makes it hard to know which of the dependent libraries are using it as well, making it unpredictable in the local context. Precisely of this reason, the global pseudo-random number generator is only suitable for uses in applications, test suites, etc. and is advised against in development of reusable libraries.

>It is also important to note that either using 'StdGen' with pure functions from other sections of this module or by relying on
'System.Random.Stateful.runStateGen' from stateful interface does not only give us deterministic behaviour without requiring 'IO', but it is also more efficient.

## Initial Takeaways
Okay, so it's pretty obvious that the library developer definitely wants you to start using the Stateful interface to play around with the global PRNG. The _random_ function and its cohorts is definitely on its way to the deprecation bin. (The Random class is listed specifically under $deprecations but it's still there in the code.) 

The _getStdGen_ function is interesting. In the 1.2.0 code, it says:

~~~haskell
-- |Gets the global pseudo-random number generator.
getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen
~~~

In the 1.2.1 code, it says:

~~~haskell
Gets the global pseudo-random number generator. Extracts the contents of 'System.Random.Stateful.globalStdGen'

getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen
~~~

Hmm. The code looks exactly the same and the type signature looks the same. So what's different? It can't be the readIORef function, because that's part of Data.IORef and simply provides a way to read the value of an IORef type, which is a mutable variable in the IO monad. So it must be the _theStdGen_ function.

In the 1.2.0 version of System.Random, the code imports the unsafePerformIO function from the System.IO.Unsafe library. It uses this function to define the function _theStdGen_. 

~~~haskell
theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}
~~~

In the 1.2.1 version of System.Random, the code does not define _theStdGen_. Instead, the function definition is moved to System.Random.Internal. 

~~~haskell
-- | Global mutable veriable with `StdGen`
theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}
~~~

But the code is the same so the result is the same. It first calls StdGen, which is a type synonym 

~~~haskell
newtype StdGen = StdGen { unStdGen :: SM.SMGen }
  deriving (Show, RandomGen, NFData)
~~~

The _SM.SMGen_ type comes from the SplitMix package, which now handles all the heavy lifting for random number generation in Haskell. Here's the definition for SMGen:

~~~haskell
data SMGen = SMGen !Word64 !Word64 -- seed and gamma; gamma is odd
  deriving Show
~~~

The two long integers (64 bit integers) provide a starting point for the MixSplit algorithm. Here are snippets from the author's paper (Guy L. Steele, Jr, Doug Lea, and Christine H. Flood):

## SplitMix Algo
Many programming language environments provide a pseudo-random number generator (PRNG), a __deterministic__ algorithm to generate a sequence of values that is likely to pass certain statistical tests for “randomness.”

>My note: The deterministic qualifier is important. It means that the algorithm produces the same sequence of values for any given starting point.

A conventional linear PRNG object provides a _generate_ method that returns one pseudorandom value and updates the state of the PRNG.

>In my reading, I kept tripping over contexts referencing the _value_ produced by a PRNG and the content of the PRNG itself. Just a heads up so you don't make the same mistake. 

A _splittable_ PRNG object has a second operation, split, that replaces the original PRNG object with two (seemingly) independent PRNG objects. It does so by creating and returning a new PRNG object while updating the state of the original PRNG object. 

>My note: As a result of the split, the original PRNG is replaced by one of the split objects while the other starts its own sequence. (I hope I'm getting these terms correct.)

Splittable PRNG objects make it easy to organize the use of pseudorandom numbers in multithreaded programs structured using fork-join parallelism.

Because the state defined for the algorithm is finite, the sequence of generated states, and therefore also the sequence
of generated values, __will necessarily repeat__, falling into a cycle (possibly after some initial subsequence that is not repeated.) In practice, PRNG algorithms are designed to
not “waste state,” so that in fact any given initial state s0
will recur). The length L of the cycle is called the _period_ of
the PRNG.

The SplittableRandom class uses 64-bit arithmetic to generate a pseudorandom sequence of 64-bit values (from which 32-bit int or 64-bit double values may then be derived.

The SplittableRandom class has two 64-bit fields called _seed_ and _gamma_. For any given instance of SplittableRandom, __the seed value is mutable and the gamma value is unchanging__. 

To ensure that every instance has period $2^{64}$, it is necessary to require that the gamma value always be odd.

The private constructor of the class is trivial (this is the authors talking), taking two arguments and using them to initialize the seed and gamma fields.

>From the code: 

>private long seed;

>private final long gamma;  // An odd integer

>private SplittableRandom(long seed,long gamma) {

>&emsp;&emsp;// Note that "gamma" should always be odd.

>&emsp;&emsp;this.seed = seed; this.gamma = gamma; }

>My note: The _final_ in the type for gamma means that once the variable has been instanced the value cannot be changed. 

The method _nextSeed_ simply adds gamma into seed and returns the result.

>private long nextSeed() {

>&emsp;&emsp; return (seed += gamma); }

A predefined gamma value is needed for initializing "root" instances of SplittableRandom (that is, instances not produced by splitting an already existing instance). We chose the odd integer closest to $2^{64}/\phi$, where $\phi = (1 + \sqrt{5} / 2)$  is the golden ratio. We call it GOLDEN_GAMMA.

>The value of GOLDEN_GAMMA in the paper and in the Haskell SplitMix source code is 0x9e3779b97f4a7c15. This yields the integer 11400714819323198485. If you work out the result of $2^{64}/\phi$, you get 
1.14007148193232E+019. Da Vinci would be so proud. 

## General thoughts on SplitMix
It was fun to dive into the recesses of SplitMix and I learned a lot. Maybe not much that I can apply directly to working with random numbers in a production setting, but I have more of a feel for how Haskell generates randomness.

### Gamma
I was intrigued by the unchangability of gamma and decided to see if I could demonstrate it. 

First, let's generate an deterministic PRNG using mkSMGen. Then use one of the sequencers in SplitMix such as _nextWord64_ or _nextInt_ to spit out a new PRNG then use that to spit out a new one and see if gamma (second integer) changes. 

~~~haskell
> pureGen = SM.mkSMGen 42
> pureGen
SMGen 9297814886316923340 13679457532755275413
--So in this particular PRNG the gamma is 13679457532755275413. 

--Now feed this to a sequencer function such as _nextWord64_.
> aa = SM.nextWord64 pureGen
> aa
(1275548033995301424,SMGen 4530528345362647137 13679457532755275413)
--This produces a tuple with the PRNG in the second position. 
--The gamma did not change but the seed did.
--That aligns with the actions described in the paper. 
--The sequencer generates a random value and replaces
--the PRNG with a new one that keeps the same gamma. 

--Now use the _snd_ function to extract the PRNG from the tuple. 
--Feed that PRNG into the sequencer. 
> ab = SM.nextWord64 (snd aa)
> ab
(10417309031967933079,SMGen 18209985878117922550 13679457532755275413)
--This produces a new random number and a new seed but the gamma value remains the same. 

--Do it again but with a different sequencer function. 
> ac = SM.nextInt (snd ab)
> ac
(2112719111588962399,SMGen 13442699337163646347 13679457532755275413)
--Okay, that proves the point. Same gamma. 
~~~

The important feature of SplitMix is the ability to take a PRNG and split it into two PRNG that are uncorrelated. It uses the 



## Where to from here?


This type signature answers the question of why StdGen has two long numbers in it:

~~~haskell
> getStdGen
StdGen {unStdGen = SMGen 7890815715975847131 14837269261089498073}
~~~

# List Comprehensions
Consider this list comprehension function:

~~~haskell
[(x,y) | x <- ['a'..'c'], y <- ['d'..'f']]

[('a','d'),('a','e'),('a','f'),('b','d'),('b','e'),('b','f'),('c','d'),('c','e'),('c','f')]
~~~

Because each of the two variables have multiple potential values (aka non-deterministic values), the comprehension operates like a Cartesian product, matching the first element in the first list to each element in the second list and so on. 




## Back to Baseline
After a reinstall of Ubuntu to smack all the nastiness I'd installed in my hacking around for random numbers,
I am installing everything methodically now. It looks to me like cabal is winning the package management
wars in Haskell, and everyone is pointing at GHCUP for installing the Haskell Tool Stack. It's not
a completely straightforward installation but it makes sense after a couple of tries. 

I like the ghcup installation because it simplifies 


First, install curl:
sudo apt install curl

Then run the install script from the GHCUp website:

https://www.haskell.org/ghcup/

run the following as a non-admin user:
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

This kicks off a script that asks whether you want to Prepend entries in the shell profile, 
and other items. Say Yes to all. 

It will then download the installer and tell you to verify a bunch of executables are installed. 
I exit at this point, copy the executable list to the clipboard, then enter "sudo apt install " and paste the list. 
Let it walk through the installs then run the script again. 

Once everything is installed, add this line to ~/.profile:

export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"

At this point, test the installation by going to ~/Documents and enter ghcup tui. This should launch the
graphical UI for ghcup. 

Highlight the latest GHC version and tap i for install. 

After installation, tap s for Set. 

Use ghc --version to verify that you're using the latest version. 





18446744073709551615
          2147483647
18446744071562067968

3709551615
2147483647


1111111111111111111111111111111111111111111111111111111111111111
                                 1111111111111111111111111111111
1111111111111111111111111111111110000000000000000000000000000000


18446744071562067968



https://www.binaryhexconverter.com/



shiftXor :: Int -> Word64 -> Word64
shiftXor n w = w `xor` (w `shiftR` n)
--Takes a Word64 value, shifts it to be a smaller value, then xor that smaller value to the Word64 value. 

Here's an example:
Select the largest available starting value for a Word64 variable: (2^64 - 1) == 18446744073709551615. 
This corresponds to a 64 bit binary number of all 1's. 
1111111111111111111111111111111111111111111111111111111111111111

Now do a _shiftR 33_ on the (2^64 - 1) value to produce a 31 bit value: (2^64 - 1) `shiftR` 33 == 2147483647. 
This corresponds to a 31 bit binary number of all 1's. 
                                 1111111111111111111111111111111

I'm using a shift value of 33 because that's what's used by SplitMix. 

Now xor the two values together, which in this case, will convert the final 31 binary digits to 0.
1111111111111111111111111111111110000000000000000000000000000000

Converting to decimal: 18446744071562067968. 

Compare the initial Word64 value to the final Word64 value. 
18446744073709551615
18446744071562067968

Note that only the final 20 digits have changed. The _shiftR 33_ function essentially split the number down the middle. 

The next function is shiftXorMultiply with three arguments: the number of bits to shift, a multiplier, and the original Word64 value. 

shiftXorMultiply :: Int -> Word64 -> Word64 -> Word64
shiftXorMultiply n k w = shiftXor n w `mult` k

In our example, we've already arrived at the value for _shiftXor 33 (2^64 - 1). It was 18446744071562067968. 

Now we multiply this value with a multiplier. SplitMix uses an algorithm called MurmerHash3. (Overview at https://en.wikipedia.org/wiki/MurmurHash. 
Source code viewable at https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp.). 
If you want to see how murmurhash3 works with various inputs, check out this site: http://murmurhash.shorelabs.com/. 

Murmurhash3 is a 128 bit algorithm. SplitMix generates a PRNG with two 64 bit values: one value called _seed_, 
the other value called _gamma_. It uses a function called _mix64_ to mash up new values for each. 

Here is the _mix64_ function from SplitMix:

mix64 :: Word64 -> Word64
mix64 z0 =
   MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3
 | 
One of the critical goals of a hashing algorithm is to produce a vary large change in the output for a very small change in the input. 
This is called _avalanche_. The fixed multipliers in the Murmur3 algorithm were selected to achieve 
as large an avalanche as possible. 

The two fixed values correspond to the following decimal and binary values:

0xff51afd7ed558ccd
18397679294719823053
1111111101010001101011111101011111101101010101011000110011001101


0xc4ceb9fe1a85ec53
14181476777654086739
1100010011001110101110011111111000011010100001011110110001010011


The SplitMix module does not expose the _mix64_ function, but you can see it in action using a little workaround. 

The module exposes a function called _seedSMGen. You can use this to feed your own seed and gamma values into
the function. 

~~~haskell
seedSMGen
    :: Word64 ^ seed
    -> Word64 ^ gamma
    -> SMGen
seedSMGen seed gamma = SMGen seed (gamma .|. 1)
~~~

The SplitMix mixing algorithm requires that the gamma value be odd, so the supplied value
is automatically bumped to an odd number using a neat trick. Since all even numbers 
in binary end in 0, if you convert the final digit to a 1, you make it odd by essentially adding 1 to the value. 
The Data.Bit module has a bitwise or operator for just this sort of purpose: _.|._ . Example: the decimal number 6 
is 110 in binary, so if you use bitwise or on the final digit, you get 111, or 7. 

~~~haskell
> 6 .|. 1 
> 7

> seedSMGen 5 6
> SMGen 5 7
~~~

The module also exposes a set of functions that are essentially sequencers. 
Given an SMGen instance (an PRNG with the SMGen type), these sequencers produce
a random value and a new PRNG instance with a different seed and the same gamma. 

To get the seed for the new PRNG, it simply adds the seed and the gamma. When you 
have nice long Word64 values, this process can be a little difficult to follow. 
But if you use _seedSMGen_ to provide simple seeds and gammas, you can watch the process in action. 
You can also see the avalanche, because each new random value is dramatically different
from the previous with only a small change in the seed value.


~~~haskell
nextWord64 :: SMGen -> (Word64, SMGen)
nextWord64 (SMGen seed gamma) = (mix64 seed', SMGen seed' gamma)
  where
    seed' = seed `plus` gamma

> aa = seedSMGen 10 1
> aa
SMGen 10 1
> nextWord64 aa
(17277706484428665540,SMGen 11 1)
> ab = nextWord64 aa
> ab
(17277706484428665540,SMGen 11 1)
> nextWord64 (snd ab)
(9868841679122378805,SMGen 12 1)
> ac = nextWord64 (snd ab)
> nextWord64 (snd ac)
(16698797833607524677,SMGen 13 1)
~~~

SplitMix exposes functions that are a bit more operationally interesting. 

The _mkSMGen_ function takes a number you provide as a seed and produces a PRNG of the SMGen type. 
Where does it get its gamma, you ask? It's hard coded as the variable goldenGamma. 

~~~haskell
goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15
~~~

You won't see this gamma value, though, because the _mkSMGen_ function scrambles it with a special
version of mix called _mixGamma at the same time that it uses _mix64_ to scramble your seed value. 

~~~haskell
mkSMGen :: Word64 -> SMGen
mkSMGen s = SMGen (mix64 s) (mixGamma (s `plus` goldenGamma))

mixGamma :: Word64 -> Word64
mixGamma z0 =
    let z1 = mix64variant13 z0 .|. 1             force to be odd
        n  = popCount (z1 `xor` (z1 `shiftR` 1))
    see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
    let's trust the text of the paper, not the code.
    in if n >= 24
        then z1
        else z1 `xor` 0xaaaaaaaaaaaaaaaa
~~~

>My note: You might, like me, find the URL in the comments intriguing. It turns out that Melissa O'Neill 
at Harvey Mudd College found an issue in the way _mixGamma_ was initially formulated in SplitMix. 

>In her paper on this subject, Ms O'Neill points out that the sample code in the original SplitMix paper
was flawed. When compared to the mixGamma function in Java, the authors inadvertently reversed
the results of the _n >= 24_ if statement. 

> Correct:
private static long mixGamma(long z) {
    z = (z ^ (z >>> 33)) * 0xff51afd7ed558ccdL; // MurmurHash3 mix constants
    z = (z ^ (z >>> 33)) * 0xc4ceabb9fe1a85ec53L;
    z = (z ^ (z >>> 33)) | 1L;                  // force to be odd
    int n = Long.bitCount(z ^ (z >>> 1));       // ensure enough transitions
    return (n < 24) ? z ^ 0xaaaaaaaaaaaaaaaaL : z;
}

> Incorrect:
private static long mixGamma(long z) {
   z = mix64variant13(z) | 1L;
   int n = Long.bitCount(z ^ (z >>> 1));
   if (n >= 24) z ^= 0xaaaaaaaaaaaaaaaaL;
   return z; }   // This result is always odd.
~~~haskell

The Java version applies the xor with a string of a's if the bitcount is less that 24. 
The paper's version applies the xor with a string of a's if the bitcount is greater than 24. 

The Haskell version of mixGamma corrected this error in version 0.0.1 of SplitMix. 
The developer left the URL to the source paper as a marker. 

Back to business. The _mkSMGen_ function applies _mix64_ to your provided seed to produce a big ol' seed value. 
It adds your provided seed to the goldenGamma value and uses mixGamma to produce a big ol' gamma value. 

So, even if you use a trival seed, such as a 1, you'll get a PRNG with big Word64 values for seed and gamma. 

~~~haskell
> mkSMGen 1
SMGen 12994781566227106604 10451216379200822465
~~~

Because the two mixers rely on fixed values inside the algorithms, you'll get this same result for _mkSMGen 1_ 
on any Haskell implementation. That's what the documentation means by _determinate_. 
If you want to get indeterminate behavior, you can use either _initSMGen_ or _newSMGen_. 
And that leads me to why I ended up in this rathole to begin with. 

Let's start with _newSMGen_. Seems like a no brainer to use this function. You run the function and 
it hands you a PRNG of the SMGen type. Run it again, you get a new PRNG (different and uncorrelated
seed and gamma values). Run it again and get another new PRNG. Nice. Simple. 

~~~haskell
> newSMGen
SMGen 17338777274666233833 17303166942003955275
> newSMGen
SMGen 17070114950890770752 5096895245633834121
> newSMGen
SMGen 6709377675803249391 17155398510583103691

> :t newSMGen
newSMGen :: IO SMGen
~~~

As you can see by the type of the result, you're now in the IO monad. Anything you do with that
SMGen instance from that point forward will need to be in the IO monad as well. 

For example, remember the sequencers such as _nextWord64_ or _nextInt_? Those are pure functions. 
If you hand them an SMGen instance, they'll fail and let you know why in no uncertain terms. 

~~~haskell
> aa = newSMGen
> :t aa
aa :: IO SMGen
> nextWord64 aa

<interactive>:119:12: error:
    • Couldn't match expected type ‘SMGen’ with actual type ‘IO SMGen’
    • In the first argument of ‘nextWord64’, namely ‘aa’
      In the expression: nextWord64 aa
      In an equation for ‘it’: it = nextWord64 aa

~~~

But, if you shove _nextWord64_ into the IO monad where it can interact with the SMGen instance,
then you'll get a result. In this example, I'll use _liftM_. If you're following along, you'll 
need to import the Control.Monad module. 

~~~haskell
> ab = newSMGen

> import Control.Monad

Control.Monad > liftM nextWord64 ab
(8663712434473332044,SMGen 15648721380286306124 4131033586674416647)
~~~

The _newSMGen_ function has a helper, _theSMGen_. 


~~~haskell
| Initialize 'SMGen' using entropy available on the system (time, ...)
initSMGen :: IO SMGen
initSMGen = fmap mkSMGen initialSeed

| Derive a new generator instance from the global 'SMGen' using 'splitSMGen'.
newSMGen :: IO SMGen
newSMGen = atomicModifyIORef theSMGen splitSMGen

theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}
~~~

Let's start at the top of the list with _initSMGen. As the comment and the type signature indicate,
this function touches the outside world via IO. But it's not immediately apparent where that happens. 
The _fmap_ function is pure, and we already saw that _mkSMGen_ is pure, so the IO must occur in initialSeed. 
But, there is no _initialSeed_ function defined in the module. What gives? 

Let's have a look at the imported modules. One of them is System.Random.SplitMix.Init. Let's have a look in there.

~~~haskell
import Data.Bits             (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Bits.Compat      (countLeadingZeros, popCount, zeroBits)
import Data.IORef            (IORef, atomicModifyIORef, newIORef)
import Data.Word             (Word32, Word64)
import System.IO.Unsafe      (unsafePerformIO)

import System.Random.SplitMix.Init
~~~

The module starts with a language specifier: {-# LANGUAGE CPP #-}. Somewhere inside this code
are functions from C++. 

The module exposes only one function: initialSeed. 

It imports:
Data.Word (Word64)
Data.Bits
import Data.Time.Clock.POSIX (getPOSIXTime)

And if GHCJS (Javascript library) is available, it imports:
Data.Word (Word32)
System.CPUTime (cpuTimePrecision, getCPUTime)

The code looks to see if the the GHC Javascript library is loaded and the Javascript SplitMix package
is defined. If yes, then the initialSeed value is set to a value generated by Math.random() 
multiplied by 2^32 (0x100000000) to make it a 32-bit number. (the Math.random() function generates
a number between 0 and 1.) 

Note: According to the readme at GitHub, the GHC Javascript compiler takes haskell source and compiles into JS rather than a GHC binary. When I get back on my Linux box I’ll download the source and see if I can compile a little random script. 

If the C library for SplitMix is available and SPLITMIX_INIT_C is defined, then
the initializer calls the _splitmix_init_ function from the library and
that value is assigned to initialSeed. 

Note: I’m really not sure how the SPLITMIX_INIT_C gets defined or linked. 

If neither of those options are available, the initializer uses POSIXTime and CPUTime to
generate a random value using this code:

~~~haskell
initialSeed =  do
    now <- getPOSIXTime
    let timebits = truncate now :: Word64
#if __GHCJS__
    let cpubits = 0
#else
    cpu <- getCPUTime
    let cpubits = fromIntegral (cpu `div` cpuTimePrecision) :: Word64
#endif
    return $ timebits `xor` cpubits
~~~

The initializer only uses cpuTime if the GHC Javascript library is not loaded and defined. 

I wondered about the two prerequisites for alternate seed sources, and couldn't get either
of them to work. 

I built a little source file to do some testing. 

~~~haskell
{-# LANGUAGE CPP #-}

#if defined(SPLITMIX_INIT_GHCJS) && __GHCJS__
initialSeed = fmap fromIntegral initialSeedJS
foreign import javascript
    "$r = Math.floor(Math.random()*0x100000000);"
    initialSeedJS :: IO Word32
#endif

#if defined(SPLITMIX_INIT_C)
initialSeed1 = initialSeedC
foreign import ccall "splitmix_init" initialSeedC :: IO Word64
#endif
~~~

In neither case did the initialSeed value initialize. Interestingly, if I moved the business
end of the code into an #else block, I got these results:

~~~haskell
--for the javascript block
error: The `javascript' calling convention is unsupported on this platform

--for the splitmix_init_c block
> initialSeed1
16780439099613034567
~~~

So it turns out that the _splitmix_init_ function is exposed to a foreign import call,
even when the library is not defined as being available. 

FWIW, seed initialization using system time and posixtime works great. 

~~~haskell
{-# LANGUAGE CPP #-}

import Data.Time.Clock.POSIX (getPOSIXTime)
#if !__GHCJS__
import System.CPUTime (cpuTimePrecision, getCPUTime)
#endif

initialSeed =  do
    now <- getPOSIXTime
    let timebits = truncate now :: Word64
#if __GHCJS__
    let cpubits = 0
#else
    cpu <- getCPUTime
    let cpubits = fromIntegral (cpu `div` cpuTimePrecision) :: Word64
#endif
    return $ timebits `xor` cpubits
~~~

The code compiles and here's the result of running my little version of initialSeed:

~~~haskell
> initialSeed
842529821
> initialSeed
995572434
> initialSeed
1034488140

> :t initialSeed
initialSeed :: IO Word64
~~~

In brief, unless I'm missing something about standard Haskell installs,
the initialSeed for SMGen most commonly comes from system time. That corresponds with a remark by one of the Random module contributors, who states that it’s better to seed your own copy of StdGen using POSIX time than to use the global StdGen PRNG. 

Let's get back to the _initSMGen_ function. 

~~~haskell
| Initialize 'SMGen' using entropy available on the system (time, ...)
initSMGen :: IO SMGen
initSMGen = fmap mkSMGen initialSeed
~~~

We've seen where initialSeed comes from and the form it takes, a Word64 value. We've already looked at _mkSMGen_. 
It is a pure function that takes a seed value and spits out an SMGen instance. Now we have a "monad moment". 
_mkSMGen_ is a pure function and initialSeed returns an _IO Word64_ value. 

~~~haskell
> mkSMGen initialSeed
error:
    • Couldn't match expected type ‘Word64’ with actual type ‘IO Word64’
~~~

And we have _fmap_ to the rescue thanks to monad magic; that is, fmap lives in both worlds, pure and monad, so you can use it to apply a pure function to a value in a monad. 

~~~haskell
> fmap mkSMGen initialSeed
SMGen 9742804028226631501 10297063185676810099

> prng = fmap mkSMGen initialSeed
> :t prng
prng :: IO SMGen
~~~

I wanted to play with this just a bit more, so I used getPOSIXTime to generate an IO POSIXTime value, then used fmap to apply functions to it. (The value of getPOSIXTime will change as the examples progress.) 

~~~haskell
import Data.Time.Clock.POSIX (getPOSIXTime)

> aa = getPOSIXTime
> aa
1637631659.455342854s

> round aa
error:
Could not deduce (RealFrac (IO POSIXTime))
        arising from a use of ‘round’
      from the context: Integral b
        bound by the inferred type of it :: Integral b => b
        at <interactive>:17:1-18

> fmap round aa
1637631729

> ab = fmap round aa
> fmap (+3000000000) ab
4637631816
~~~

That's it then. The _initialSeed_ function grabs a big ol' number from the system clocks and _initSMGen_ uses that value as a seed for _mkSMGen_ to produce a PRNG of the IO SMGen type. So we're still in the IO monad. 

Now let's deal with _newSMGen_, and particularly that helper function in the middle: _theSMGen. 

~~~haskell
newSMGen :: IO SMGen
newSMGen = atomicModifyIORef theSMGen splitSMGen

theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}
~~~

In the function definition for _theSMGen_ there's this ugly wart called _unsafePerformIO_. It's imported at the top of the SplitMix module. I'm not even close to being able 
to follow its interactions within the IO monad. Here are some snippets from the System.IO.Unsafe source:

~~~haskell
This is the \"back door\" into the 'IO' monad, allowing
'IO' computation to be performed at any time.  For
this to be safe, the 'IO' computation should be
free of side effects and independent of its environment.

* Use @{\-\# NOINLINE foo \#-\}@ as a pragma on any function @foo@
        that calls 'unsafePerformIO'.  If the call is inlined,
        the I\/O may be performed more than once.
~~~

It turns out that the only function in System.IO.Unsafe is _unsafeFixIO_. The other functions:
 unsafePerformIO
 unsafeDupablePerformIO
 unsafeInterleaveIO

are in the GHC.IO package under GHC.IO.Unsafe. Here are the functions:

~~~haskell
--module GHC.IO.Unsafe

unsafePerformIO :: IO a -> a
unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)

unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = case runRW# m of (# _, a #) -> a

noDuplicate :: IO ()
noDuplicate = IO $ \s -> case noDuplicate# s of s' -> (# s', () #)

--From the code comments:
This version of 'unsafePerformIO' is more efficient
because it omits the check that the IO is only being performed by a
single thread.  Hence, when you use 'unsafeDupablePerformIO',
there is a possibility that the IO action may be performed multiple
times (on a multiprocessor), and you should therefore ensure that
it gives the same results each time.

noDuplicate ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.
~~~

The best I can digest from that is there is something __unsafe__ about the way _initSMGen_ works 
with local system resources. Even though it returns an IO SMGen value, that's not sufficient to protect 
the functional operation of SplitMix. So another layer of operation is put in place. 

I did some reading on this subject and it looks like there are a variety of issues that come up
when you start meddling with mutable variables in a multithreaded environment or with multiple apps 
running at the same time. These concurrency and atomization concerns focus on the use 
of the global PRNG, which resides in a mutable variable. 

The _unsafePerformIO_ function is essentially a wrapper around a safer function, _unsafeDupablePerformIO_, 
which itself makes use of a concurrency protection function called _noDuplicate_. 

Here are code comments for _noDuplicate_:

~~~haskell
Ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.
~~~

I snipped the functions into a separate little file and walked through them
to make sure I understood what was going on. 

~~~haskell
goldenGamma :: Word64
goldenGamma = 0x9e3779b97f4a7c15

mix64 :: Word64 -> Word64
mix64 z0 =
   MurmurHash3Mixer
    let z1 = shiftXorMultiply 33 0xff51afd7ed558ccd z0
        z2 = shiftXorMultiply 33 0xc4ceb9fe1a85ec53 z1
        z3 = shiftXor 33 z2
    in z3

mix64variant13 :: Word64 -> Word64
mix64variant13 z0 =
   Better Bit Mixing - Improving on MurmurHash3's 64-bit Finalizer
   http://zimbry.blogspot.fi/2011/09/better-bit-mixing-improving-on.html
   Stafford's Mix13
    let z1 = shiftXorMultiply 30 0xbf58476d1ce4e5b9 z0 MurmurHash3 mix constants
        z2 = shiftXorMultiply 27 0x94d049bb133111eb z1
        z3 = shiftXor 31 z2
    in z3

mixGamma :: Word64 -> Word64
mixGamma z0 =
    let z1 = mix64variant13 z0 .|. 1             force to be odd
        n  = popCount (z1 `xor` (z1 `shiftR` 1))
    in if n >= 24
        then z1
        else z1 `xor` 0xaaaaaaaaaaaaaaaa

mkSMGen :: Word64 -> SMGen
mkSMGen s = SMGen (mix64 s) (mixGamma (s `plus` goldenGamma))


initialSeed =  do
    now <- getPOSIXTime
    let timebits = truncate now :: Word64
    cpu <- getCPUTime
    let cpubits = fromIntegral (cpu `div` cpuTimePrecision) :: Word64
    return $ timebits `xor` cpubits



initSMGen :: IO SMGen
initSMGen = fmap mkSMGen initialSeed



theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}


getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen

~~~

~~~haskell
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude
           , MagicHash
           , UnboxedTuples
  #-}

import GHC.Base
import GHC.IO
--import GHC.IORef
import Data.IORef (atomicModifyIORef, IORef, newIORef)
import GHC.Exception
import Control.Exception
import System.Random.SplitMix as SM
import Data.Time.Clock.POSIX (getPOSIXTime)


unsafePerformIO' :: IO a -> a
unsafePerformIO' m = unsafeDupablePerformIO' (noDuplicate >> m)

unsafeDupablePerformIO'  :: IO a -> a
unsafeDupablePerformIO' (IO m) = case runRW# m of (# _, a #) -> a

undupIO :: IO a -> a
undupIO (IO m) = case runRW# m of (# _, a #) -> a

theSMGen' :: IORef SMGen
theSMGen' = unsafePerformIO' $ SM.initSMGen >>= newIORef 
{-# NOINLINE theSMGen' #-}

myinit = SM.initSMGen
myioref = myinit >>= newIORef
mythesmg = undupIO (myioref)

newSMGen' :: IO SMGen
newSMGen' = atomicModifyIORef theSMGen' splitSMGen

mynewsmg = atomicModifyIORef mythesmg splitSMGen

~~~

Here are the types of the resulting variables. 

~~~haskell
> :t myinit
myinit :: IO SMGen
> :t myioref
myioref :: IO (IORef SMGen)
> :t mythesmg
mythesmg :: IORef SMGen
> :t mynewsmg
mynewsmg :: IO SMGen
> mynewsmg
SMGen 13395869693777320398 5574149941147275857

~~~

First, use the initSMGen function in the SplitMix module to generate a PRNG with type IO SMGen. 
Next, feed the IO SMGen type into newIORef to produce ... what? 

According to Haskell High Performance Programming, it's producing a single mutable reference with no locking. 
In this context, mutable means an area of shared memory. So the type _IO (IORef SMGen)_ must indicate that
the SMGen value inside the IO SMGen type has been lifted into an IORef while the whole sheebang
still resides in the IO monad. 

The unsafeDupablePerformIO' function had this unfamiliar use of the hash (#) symbol.

~~~haskell
unsafeDupablePerformIO'  :: IO a -> a
unsafeDupablePerformIO' (IO m) = case runRW# m of (# _, a #) -> a
~~~

Turns out that this symbol gives a signal to the GHC preprocessor to unbox the associated function or variable. 

Here are links to documentation and research matter:

https://downloads.haskell.org/~ghc/9.2.1/docs/html/users_guide/exts/primitives.html

6.16. Unboxed types and primitive operations

GHC is built on a raft of primitive data types and operations; 
“primitive” in the sense that they cannot be defined in Haskell itself. 
While you really can use this stuff to write fast code, we generally find it 
a lot less painful, and more satisfying in the long run, to use higher-level 
language features and libraries. With any luck, the code you write will be optimised 
to the efficient unboxed version in any case. And if it isn’t, we’d like to know about it.

https://wiki.haskell.org/Unboxed_type

Unboxed types are types that represent raw values. Unboxed types have kind #. Note that unboxed 
types of different storage behaviours (four bytes, eight bytes etc.) 
are all lumped together under kind #. As a result, type variables must have kinds which are #-free. 

https://www.fpcomplete.com/haskell/tutorial/primitive-haskell/

The point of this chapter is to help you peel back some of the layers of abstraction in Haskell coding, 
with the goal of understanding things like primitive operations, evaluation order, and mutation. 
Some concepts covered here are generally "common knowledge" in the community, while others are less 
well understood. The goal is to cover the entire topic in a cohesive manner. 
If a specific section seems like it's not covering anything you don't already know, 
skim through it and move on to the next one.

While this chapter is called "Primitive Haskell," the topics are very much GHC-specific. 
I avoided calling it "Primitive GHC" for fear of people assuming it was about the internals of GHC itself. 
To be clear: these topics apply to anyone compiling their Haskell code using the GHC compiler.

https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/prim-ops

Primitive Operations (PrimOps)
PrimOps are functions that cannot be implemented in Haskell, and are provided natively by GHC.  
For example, adding two Int# values is provided as the PrimOp +#, and allocating a new mutable array 
is the PrimOp newArray#.
PrimOps are made available to Haskell code through the virtual module GHC.Prim.  
This module has no implementation, and its interface never resides on disk: if GHC.Prim is imported, 
we use a built-in ModIface value - see ghcPrimIface in compiler/GHC/Iface/Load.hs.

Summary

All these references boil down to the essential difference between boxed and unboxed values. 
The normal Haskell handling of a value (or a function or a tuple) is to stuff it in memory
and use a pointer for access. (I'm not sure how the compiler handles the type designation
for that pointer.) Here's an example I thought up myself. 

~~~haskell
> map (*2) [1..5]
[2,4,6,8,10]
> x = 1
> y = 5
> map (*2) [x..y]
[2,4,6,8,10]
~~~

An unboxed value (etc) is a bare memory location. For this reason, it can't be polymorphic. 
A tuple is unboxed by putting the hash symbol at the interiors of the parens (# ... #). 

Here's the line from the SplitMix library again: 

~~~haskell
unsafeDupablePerformIO' (IO m) = case runRW# m of (# _, a #) -> a
~~~

The _runRW#_ function is in the GHC.Magic library. 
https://hackage.haskell.org/package/ghc-prim-0.5.1.0/docs/GHC-Magic.html

~~~haskell
runRW# :: forall (r :: RuntimeRep) (o :: TYPE r). (State# RealWorld -> o) -> o

Apply a function to a 'State# RealWorld' token. When manually applying a function to realWorld#, 
it is necessary to use NOINLINE to prevent semantically undesirable floating. runRW# is inlined, 
but only very late in compilation after all floating is complete.
~~~

Just about every term in that paragraph is gibberish to me. 

What is a 'State# RealWorld' token? 

The State# type is defined in the GHC.Prim library. It is located under a heading 
called 'Concurrency primitives'. 

data State# s

State# is the primitive, unlifted type of states. It has one type parameter, 
thus State# RealWorld, or State# s, where s is a type variable. 
The only purpose of the type parameter is to keep different state threads separate. 
It is represented by nothing at all. 

The RealWorld type is also defined in GHC.Prim. 

~~~haskell
data RealWorld

RealWorld is deeply magical. It is primitive, but it is not unlifted (hence ptrArg). 
We never manipulate values of type RealWorld; it's only used in the type system, to parameterise State#. 

Who the heck knows what they mean by "deeply magical". Sometimes I really hate Haskell docs. I'm 
guessing that "magical" means that the underlying C++ code in GHC is doing things that
are not visible from Haskell. 
~~~

There's a function called realWorld# in the RealWorld package but it looks like it's been 
superceded by runRW#. 


What does 'semantically undesirable floating' mean? 

Still no clue. 

What does inlining after float complete mean? 

Still not sure. 


With the IO vagarities out of the way, the _unsafePerformIO_ function acts on the result 
of running initSMGen then shoving that into IORef. 


~~~haskell
> mySMGen = initSMGen
> mySMGen
SMGen 13168896641437015400 10282130754770673047
> :t mySMGen
mySMGen :: IO SMGen

> mySMGen >>= newIORef
> :t mySMGen >>= newIORef
mySMGen >>= newIORef :: IO (IORef SMGen)
> myIOGen = mySMGen >>= newIORef
> :t myIOGen
myIOGen :: IO (IORef SMGen)

> :t atomicModifyIORef
atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b

> atomicModifyIORef myIOGen splitSMGen
SMGen 10078639513945947020 7248417822788667305

> finalGen = atomicModifyIORef myIOGen splitSMGen
> finalGen
SMGen 13633800400169909639 16551613339100862019

> :t finalGen
finalGen :: IO SMGen

~~~

I'm really not completely sure why we had to go down that road simply to convert
an IO SMGen instance to another IO SMGen instance. It must be the way it's used in the Random or Random.Internal
package. The SplitMix functions are called by functions that generate Standard Gen (StdGen)
PRNG instances. Here are examples:

From System.Random:

~~~haskell
initStdGen :: MonadIO m => m StdGen
initStdGen = liftIO (StdGen <$> SM.initSMGen)
~~~


These functions in System.Random use _theStdGen_ from Internal
and appear to be around for backward compat:

~~~haskell
setStdGen :: MonadIO m => StdGen -> m ()
setStdGen = liftIO . writeIORef theStdGen

getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen

newStdGen :: MonadIO m => m StdGen
newStdGen = liftIO $ atomicModifyIORef' theStdGen split

getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
getStdRandom f = liftIO $ atomicModifyIORef' theStdGen (swap . f)
  where swap (v, g) = (g, v)
~~~


More functions exported by System.Random


~~~haskell
> randomRIO
randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
randomRIO range = getStdRandom (randomR range)

| A variant of 'System.Random.Stateful.randomRM' that uses the global
pseudo-random number generator 'System.Random.Stateful.globalStdGen'
--
>>> randomRIO (2020, 2100) :: IO Int
2040
--
Similar to 'randomIO', this function is equivalent to @'getStdRandom'
'randomR'@ and is included in this interface for historical reasons and
backwards compatibility. It is recommended to use
'System.Random.Stateful.uniformRM' instead, possibly with the
'System.Random.Stateful.globalStdGen' if relying on the global state is
acceptable.
--
>>> import System.Random.Stateful
>>> uniformRM (2020, 2100) globalStdGen :: IO Int
2079

> randomIO
randomIO :: (Random a, MonadIO m) => m a
randomIO = getStdRandom random

| A variant of 'System.Random.Stateful.randomM' that uses the global
pseudo-random number generator 'System.Random.Stateful.globalStdGen'.
--
>>> import Data.Int
>>> randomIO :: IO Int32
-1580093805
--
This function is equivalent to @'getStdRandom' 'random'@ and is included in
this interface for historical reasons and backwards compatibility. It is
recommended to use 'System.Random.Stateful.uniformM' instead, possibly with
the 'System.Random.Stateful.globalStdGen' if relying on the global state is
acceptable.
--
>>> import System.Random.Stateful
>>> uniformM globalStdGen :: IO Int32
-1649127057

~~~


These functions are in System.Random.Internal (imported into System.Random) and
were moved there in version 1.2.1:

~~~haskell
newtype StdGen = StdGen { unStdGen :: SM.SMGen }
  deriving (Show, RandomGen, NFData)
  
mkStdGen :: Int -> StdGen
mkStdGen = StdGen . SM.mkSMGen . fromIntegral

theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}

~~~

SplitMix is not imported into System.Random.Stateful. 

These interesting functions are in System.Random.Stateful (imported into System.Random)

~~~haskell
globalStdGen :: AtomicGenM StdGen
globalStdGen = AtomicGenM theStdGen

newIOGenM :: MonadIO m => g -> m (IOGenM g)
newIOGenM = fmap IOGenM . liftIO . newIORef


~~~

Okay, so I've spent about a week on this one little bit of code and many ratholes in random number generation
and I don't think I'm much further down the road then when I started. But I'm a bit smarter on why
Haskell appears to be such a jumble of randomization options. 

Let's see if I can get my thoughts sorted out. 

The System.Random.Internal module has three interfaces (classes??)

RandomGen
StatefulGen
FrozenGen

The RandomGen interface provides the class that contains StdGen

The _next_ and _genRange_ functions under RandomGen are deprecated. 

~~~haskell
| 'RandomGen' is an interface to pure pseudo-random number generators.
'StdGen' is the standard 'RandomGen' instance provided by this library.
@since 1.0.0
~~~

There appear to be a couple of buckets. One bucket holds all the ways to generate a PRNG. The other bucket
holds all the ways to generate a random value using a PRNG. 

Both buckets are divided into pure and monadic compartments. 

~~~haskell
  module System.Random
  (
  * Pure number generator interface
  $interfaces
    RandomGen(..)
  , uniform
  , uniformR
  , genByteString
  , Random(..)
  , Uniform
  , UniformRange
  , Finite

  ** Standard pseudo-random number generator
  , StdGen
  , mkStdGen
  , initStdGen

  ** Global standard pseudo-random number generator
  $globalstdgen
  , getStdRandom
  , getStdGen
  , setStdGen
  , newStdGen
  , randomIO
  , randomRIO

  ) where
~~~

Several of these functions such as mkStdGen, along with the RandomGen class and several types such as StdGen, 
Uniform, and UniformRange are imported from System.Random.Internal and re-exported. 

~~~haskell
module System.Random.Internal
  (* Pure and monadic pseudo-random number generator interfaces
    RandomGen(..)
  , StatefulGen(..)
  , FrozenGen(..)

  ** Standard pseudo-random number generator
  , StdGen(..)
  , mkStdGen
  , theStdGen

  * Monadic adapters for pure pseudo-random number generators
  ** Pure adapter
  , StateGen(..)
  , StateGenM(..)
  , splitGen
  , runStateGen
  , runStateGen_
  , runStateGenT
  , runStateGenT_
  , runStateGenST
  , runStateGenST_

  * Pseudo-random values of various types
  , Uniform(..)
  , uniformViaFiniteM
  , UniformRange(..)
  , uniformByteStringM
  , uniformDouble01M
  , uniformDoublePositive01M
  , uniformFloat01M
  , uniformFloatPositive01M
  , uniformEnumM
  , uniformEnumRM

  * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  ) where
~~~

Interesting note. I've been going back and forth between Stack and Cabal and when I do that, I inadvertently shift
from using Random 1.2.0 (Stack) to Random 1.2.1 (Cabal). In Random 1.2.0, the initStdGen function is not exported. Drove me
crazy for a while. Didn't know why it would sometimes work and sometimes not work. 

The _(..)_ after the _RandomGen_ class and _Random_ class indicates that all functions
in that class are exported. This is confirmed using :browse in GHC:

~~~haskell
type Random :: * -> Constraint
class Random a where
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  default randomR :: (RandomGen g, UniformRange a) =>
                     (a, a) -> g -> (a, g)
  random :: RandomGen g => g -> (a, g)
  default random :: (RandomGen g, Uniform a) => g -> (a, g)
  randomRs :: RandomGen g => (a, a) -> g -> [a]
  randoms :: RandomGen g => g -> [a]
genByteString ::
  RandomGen g =>
  Int
  -> g
  -> (bytestring-0.11.1.0:Data.ByteString.Internal.ByteString, g)
getStdGen :: Control.Monad.IO.Class.MonadIO m => m StdGen
getStdRandom ::
  Control.Monad.IO.Class.MonadIO m => (StdGen -> (a, StdGen)) -> m a
initStdGen :: Control.Monad.IO.Class.MonadIO m => m StdGen
newStdGen :: Control.Monad.IO.Class.MonadIO m => m StdGen
randomIO :: (Random a, Control.Monad.IO.Class.MonadIO m) => m a
randomRIO ::
  (Random a, Control.Monad.IO.Class.MonadIO m) => (a, a) -> m a
setStdGen :: Control.Monad.IO.Class.MonadIO m => StdGen -> m ()
uniform :: (RandomGen g, Uniform a) => g -> (a, g)
uniformR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
type Finite :: * -> Constraint
class Finite a where
  random-1.2.1:System.Random.GFinite.cardinality :: ghc-prim-0.8.0:GHC.Prim.Proxy#
                                                      a
                                                    -> random-1.2.1:System.Random.GFinite.Cardinality
  default random-1.2.1:System.Random.GFinite.cardinality :: (GHC.Generics.Generic
                                                               a,
                                                             random-1.2.1:System.Random.GFinite.GFinite
                                                               (GHC.Generics.Rep a)) =>
                                                            ghc-prim-0.8.0:GHC.Prim.Proxy# a
                                                            -> random-1.2.1:System.Random.GFinite.Cardinality
  random-1.2.1:System.Random.GFinite.toFinite :: Integer -> a
  default random-1.2.1:System.Random.GFinite.toFinite :: (GHC.Generics.Generic
                                                            a,
                                                          random-1.2.1:System.Random.GFinite.GFinite
                                                            (GHC.Generics.Rep a)) =>
                                                         Integer -> a
  random-1.2.1:System.Random.GFinite.fromFinite :: a -> Integer
  default random-1.2.1:System.Random.GFinite.fromFinite :: (GHC.Generics.Generic
                                                              a,
                                                            random-1.2.1:System.Random.GFinite.GFinite
                                                              (GHC.Generics.Rep a)) =>
                                                           a -> Integer
type RandomGen :: * -> Constraint
class RandomGen g where
  next :: g -> (Int, g)
  genWord8 :: g -> (GHC.Word.Word8, g)
  genWord16 :: g -> (GHC.Word.Word16, g)
  genWord32 :: g -> (GHC.Word.Word32, g)
  genWord64 :: g -> (GHC.Word.Word64, g)
  genWord32R :: GHC.Word.Word32 -> g -> (GHC.Word.Word32, g)
  genWord64R :: GHC.Word.Word64 -> g -> (GHC.Word.Word64, g)
  genShortByteString :: Int
                        -> g
                        -> (bytestring-0.11.1.0:Data.ByteString.Short.Internal.ShortByteString,
                            g)
  genRange :: g -> (Int, Int)
  split :: g -> (g, g)
  {-# MINIMAL split, (genWord32 | genWord64 | next, genRange) #-}
type StdGen :: *
newtype StdGen
  = System.Random.Internal.StdGen {System.Random.Internal.unStdGen :: splitmix-0.1.0.4:System.Random.SplitMix.SMGen}
type Uniform :: * -> Constraint
class Uniform a where
  System.Random.Internal.uniformM :: System.Random.Internal.StatefulGen
                                       g m =>
                                     g -> m a
  default System.Random.Internal.uniformM :: (System.Random.Internal.StatefulGen
                                                g m,
                                              GHC.Generics.Generic a,
                                              System.Random.Internal.GUniform
                                                (GHC.Generics.Rep a)) =>
                                             g -> m a
type UniformRange :: * -> Constraint
class UniformRange a where
  System.Random.Internal.uniformRM :: System.Random.Internal.StatefulGen
                                        g m =>
                                      (a, a) -> g -> m a
  {-# MINIMAL uniformRM #-}
mkStdGen :: Int -> StdGen

~~~

As another side note, knowing what I know now, I would have started off by making a print
of the source code with the exported functions listed so I could go through them one by one
and figure out why they were there and how to use them. 

~~~haskell
uniform :: (Uniform a, RandomGen g) => g -> (a, g)
uniform g = runStateGen g uniformM
{-# INLINE uniform #-}

uniformR :: (UniformRange a, RandomGen g) => (a, a) -> g -> (a, g)
uniformR r g = runStateGen g (uniformRM r)
{-# INLINE uniformR #-}

genByteString :: RandomGen g => Int -> g -> (ByteString, g)
genByteString n g = runStateGenST g (uniformByteStringM n)
{-# INLINE genByteString #-}

class Random a where

  {-# INLINE randomR #-}
  randomR :: RandomGen g => (a, a) -> g -> (a, g)
  default randomR :: (RandomGen g, UniformRange a) => (a, a) -> g -> (a, g)
  randomR r g = runStateGen g (uniformRM r)

  {-# INLINE random #-}
  random  :: RandomGen g => g -> (a, g)
  default random :: (RandomGen g, Uniform a) => g -> (a, g)
  random g = runStateGen g uniformM

  {-# INLINE randomRs #-}
  randomRs :: RandomGen g => (a,a) -> g -> [a]
  randomRs ival g = build (\cons _nil -> buildRandoms cons (randomR ival) g)

  {-# INLINE randoms #-}
  randoms  :: RandomGen g => g -> [a]
  randoms  g      = build (\cons _nil -> buildRandoms cons random g)

initStdGen :: MonadIO m => m StdGen
initStdGen = liftIO (StdGen <$> SM.initSMGen)

setStdGen :: MonadIO m => StdGen -> m ()
setStdGen = liftIO . writeIORef theStdGen

getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen

newStdGen :: MonadIO m => m StdGen
newStdGen = liftIO $ atomicModifyIORef' theStdGen split

getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
getStdRandom f = liftIO $ atomicModifyIORef' theStdGen (swap . f)
  where swap (v, g) = (g, v)

randomRIO :: (Random a, MonadIO m) => (a, a) -> m a
randomRIO range = getStdRandom (randomR range)

randomIO :: (Random a, MonadIO m) => m a
randomIO = getStdRandom random

~~~

A separate module, not imported into either System.Random nor System.Random.Internal, is
System.Random.Stateful. Based on many of the comments in the code and in Git discussions,
the development team really really wants you to use Stateful functions. 

~~~haskell
module System.Random.Stateful
  (
  $interfaces
  , StatefulGen(..)
  , FrozenGen(..)
  , RandomGenM(..)
  , withMutableGen
  , withMutableGen_
  , randomM
  , randomRM
  , splitGenM

  * Monadic adapters for pure pseudo-random number generators #monadicadapters#
  $monadicadapters

  ** Pure adapter
  , StateGen(..)
  , StateGenM(..)
  , runStateGen
  , runStateGen_
  , runStateGenT
  , runStateGenT_
  , runStateGenST
  , runStateGenST_
  ** Mutable adapter with atomic operations
  , AtomicGen(..)
  , AtomicGenM(..)
  , newAtomicGenM
  , applyAtomicGen
  , globalStdGen
  ** Mutable adapter in 'IO'
  , IOGen(..)
  , IOGenM(..)
  , newIOGenM
  , applyIOGen
  ** Mutable adapter in 'ST'
  , STGen(..)
  , STGenM(..)
  , newSTGenM
  , applySTGen
  , runSTGen
  , runSTGen_
  ** Mutable adapter in 'STM'
  , TGen(..)
  , TGenM(..)
  , newTGenM
  , newTGenMIO
  , applyTGen

  * Pseudo-random values of various types
  $uniform
  , Uniform(..)
  , uniformListM
  , uniformViaFiniteM
  , UniformRange(..)

  * Generators for sequences of pseudo-random bytes
  , genShortByteStringIO
  , genShortByteStringST
  , uniformByteStringM
  , uniformDouble01M
  , uniformDoublePositive01M
  , uniformFloat01M
  , uniformFloatPositive01M
  , uniformEnumM
  , uniformEnumRM

  ) where

~~~




In the monadic compartment, there are various types of monads and ways to pass a generator 
between them. 

Also in the monadic compartment, there is a global PRNG accessed via IO. 

~~~haskell
There is a single, implicit, global pseudo-random number generator of type
'StdGen', held in a global mutable variable that can be manipulated from
within the 'IO' monad. It is also available as
'System.Random.Stateful.globalStdGen', therefore it is recommended to use the
new "System.Random.Stateful" interface to explicitly operate on the global
pseudo-random number generator.
--
It is initialised with 'initStdGen', although it is possible to override its
value with 'setStdGen'. All operations on the global pseudo-random number
generator are thread safe, however in presence of concurrency they are
naturally become non-deterministic. Moreover, relying on the global mutable
state makes it hard to know which of the dependent libraries are using it as
well, making it unpredictable in the local context. Precisely of this reason,
the global pseudo-random number generator is only suitable for uses in
applications, test suites, etc. and is advised against in development of
reusable libraries.
--
It is also important to note that either using 'StdGen' with pure functions
from other sections of this module or by relying on
'System.Random.Stateful.runStateGen' from stateful interface does not only
give us deterministic behaviour without requiring 'IO', but it is also more
efficient.

~~~

The blurb says that the global PRNG is initialized with the _initStdGen_ function. 
Here is that function:

~~~haskell
| Initialize 'StdGen' using system entropy (i.e. @\/dev\/urandom@) when it is
available, while falling back on using system time as the seed.
--
@since 1.2.1
initStdGen :: MonadIO m => m StdGen
initStdGen = liftIO (StdGen <$> SM.initSMGen)

> initStdGen
StdGen {unStdGen = SMGen 6312780494390792821 7322586672831735457}

> initSM = initSMGen
> initStd = initStdGen

> :t initSM
initSM :: IO SMGen

> :t initStd
initStd :: MonadIO m => m StdGen

~~~

The _initStdGen_ function essentially runs initSMGen from the SplitMix package,
wraps the _IO SMGen_ result in the StdGen type, and uses liftIO to put it into a generalized monoidal
context ready for whatever function will call it. 

The System.Random.Stateful module has this to say about the _globalStdGen_ "interface":

~~~haskell
| Global mutable standard pseudo-random number generator. This is the same
generator that was historically used by `randomIO` and `randomRIO` functions.
--
>>> replicateM 10 (uniformRM ('a', 'z') globalStdGen)
"tdzxhyfvgr"
--
@since 1.2.1
globalStdGen :: AtomicGenM StdGen
globalStdGen = AtomicGenM theStdGen

~~~

Note that this "interface" has only been around since version 1.2.1, which is the current version
(as I write this in Nov 2021). 

Here now is a beast we've seen before, in a way. _theStdGen_ resides in System.Random.Internal and 
looks like this (I've reprised the definition of _theSMGen_ for comparison.

~~~haskell
--from System.Random.Internal:
theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}

--from System.Random.SplitMix
theSMGen :: IORef SMGen
theSMGen = unsafePerformIO $ initSMGen >>= newIORef
{-# NOINLINE theSMGen #-}
~~~

They are essentially the same function, the only difference being that _theStdGen_ stuffs the 
result of calling _unsafePerformIO $ SM.initSMGen_ into the StdGen type. 

Because they both rely on _initSMGen_, they both have the same restrictions. Concurrency can
be a problem. In fact, some controversy surrounds use of the Global PRNG 
precisely because of the shared nature of the source. This is from a GitHub discussion on the subject,
where one of the contributors, Alexey Kuleshevich, describes the potential issues with
using a mutable global variable such as the Global PRNG:

~~~haskell

The global random number generator ... is defined as 
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen, 
which means every library that you depend on in your program 
can use the same generator behind the scenes.

Using global mutable variables like that should be the last resort in Haskell. 
And in case of pseudo random number generator it is definitely uncalled for. 
If StdGen is needed at some point in the code base, it is better to get some entropy 
from the system and initialize a new StdGen and use that in your own code base. 
After you get a new StdGen (or any other RandomGen/StatefulGen) initialized 
you can use it in whichever way you feel is more appropriate for you, 
either with monadic mutable approach or pure state passing it does not matter.

So, this mutates global state shared by your whole program. 
Very bad. 

This [example] gives you your own StdGen that is available in your local scope.

gen <- newIOGenM . mkStdGen . round  =<< getPOSIXTime
xs <- uniformListM n gen :: IO [Int]
So, if you are writing a library that needs randomness, you should ask for a generator, instead of silently using the global one. And that is why I'd rather deprecate it and prevent it from being abused period.

~~~

Here's a snippet that uses his suggestion:

~~~haskell
import System.Random 
import System.Random.Stateful 
import System.Random.Internal
import Data.Word
import Data.IORef (IORef, atomicModifyIORef, newIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe      (unsafePerformIO)

> getPOSIXTime
1637284219.014361402s
> liftM round getPOSIXTime
1637284345
> gen = newIOGenM . mkStdGen . round =<< getPOSIXTime
> :t gen
gen :: IO (IOGenM StdGen)

~~~


He goes on to describe a kind of convoluted example but one that demonstrates how 
the global PRNG could cause an issue:

~~~haskell
Almost every program can be restructured to not use global mutable variables. It is a known hack and a lot of people think it is ok to use. Because of that I've seen quite a few complex pieces of software where such variables are being used. It is very hard to reason about global variables because they are located in a global scope and can be mutated from anywhere and debugging issues related to such variables becomes very hard. I feel so strongly about it from experience, not because it just looks ugly.

When such variable is being exported by a library this problem is amplified, because all it takes is any of the libraries you depend on to make a silly bug like this:

λ> setStdGen undefined
And you will get this error in any other place in your code. Reasoning where this bottom value came from will be extremely hard because it could have happen anywhere in the code that you didn't even write. All you will get is this:

λ> i <- randomIO :: IO Int
*** Exception: Prelude.undefined
This is example might seem silly, but let's obfuscate it a bit.

Consider this innocent function that ensures that a random value that is generated does not equal to the one we pass to it. Also imagine that you didn't write this function and it happens to live in some library that you use:

randomWithRejection :: (Random a, RandomGen g, Eq a) => a -> g -> (a, g)
randomWithRejection x g =
  case random g of
    (x', g')
      | x /= x' -> (x', g')
      | otherwise -> randomWithRejection x g
And it happens to be used in way you suggest is OK with the global generator:

randomWithRejectionIO :: (Random a, Eq a) => a ->IO a
randomWithRejectionIO x = getStdRandom (randomWithRejection x)
And that function is further used in some piece of code that is accepted to not terminate and it happens we get the right random value:

λ> setStdGen (mkStdGen 217)
...
λ> timeout 500000 $ randomWithRejectionIO (-2069219337697363530 :: Int)
Nothing
At this point after so many very unlikely conditions have been met using global random number generator in any other place will simply deadlock.

λ> randomIO :: IO Int
Interrupted.
Debugging this sort of problem will be close to impossible, because this bug is not only non-deterministic, but also has a global scope.
~~~

With all this being said, I'm going to put a placeholder in my brain to avoid using
the Global PRNG. 

According to the comments in System.Random, the _getStdGen_ function "extracts" the contents 
of _globalStdGen_ in the Stateful module. 

~~~haskell
| Gets the global pseudo-random number generator. Extracts the contents of
'System.Random.Stateful.globalStdGen'
--
@since 1.0.0
getStdGen :: MonadIO m => m StdGen
getStdGen = liftIO $ readIORef theStdGen
~~~

But, from what I can see, it doesn't really do that. It calls directly on _theStdGen_, which
is in System.Internal and not System.Stateful. When it says "extract", maybe it's 
simply referring to the fact that both System.Stateful.globalStdGen and System.Random.getStdGen
both call on System.Internal.theStdGen. 

The difference is that _globalStdGen_ defines the result of _theStdGen_ as the type AtomicGenM. 
This was a little bit of a learning experience for me, because I'm not experienced with
type functions. 

~~~haskell
> :t AtomicGenM
AtomicGenM :: IORef g -> AtomicGenM g
~~~

AtomicGenM takes an IORef type and makes it an AtomicGenM type. And as I've seen before, _theStdGen_
is an IORef type. 

~~~haskell
theStdGen :: IORef StdGen
theStdGen = unsafePerformIO $ SM.initSMGen >>= newIORef . StdGen
{-# NOINLINE theStdGen #-}
~~~

If I'm reading Parallel and Concurrent Programming in Haskell correctly, the advantage of "converting"
_theStdGen_ to AtomicGenM is to make it safer for concurrent processing. The atomicity assures
that the value of the underlying globalStdGen value isn't changed by some other process 
for those functions that modify the original PRNG when they generate a new one. 

There is another function in System.Random that makes use of _theStdGen_: the _getStdRandom_ function. 

~~~haskell
getStdRandom :: MonadIO m => (StdGen -> (a, StdGen)) -> m a
getStdRandom f = liftIO $ atomicModifyIORef' theStdGen (swap . f)
  where swap (v, g) = (g, v)
~~~

The source doc states this function is "outdated."

~~~haskell
This is an outdated function and it is recommended to switch to its
equivalent 'System.Random.Stateful.applyAtomicGen' instead, possibly with the
'System.Random.Stateful.globalStdGen' if relying on the global state is
acceptable.
--
>>> import System.Random.Stateful
>>> rollDice = applyAtomicGen (uniformR (1, 6)) globalStdGen
>>> replicateM 10 (rollDice :: IO Int)
[4,6,1,1,4,4,3,2,1,2]
--
~~~

The _applyAtomicGen_ function from Stateful atomically applies a pure operation 
to the wrapped pseudo-random number generator.

~~~haskell
applyAtomicGen :: MonadIO m => (g -> (a, g)) -> AtomicGenM g -> m a
applyAtomicGen op (AtomicGenM gVar) =
  liftIO $ atomicModifyIORef' gVar $ \g ->
    case op g of
      (a, g') -> (g', a)
{-# INLINE applyAtomicGen #-}
~~~

The comments give this example:

~~~haskell
>>> import System.Random.Stateful
>>> let pureGen = mkStdGen 137
>>> g <- newAtomicGenM pureGen
>>> applyAtomicGen random g :: IO Int
7879794327570578227
--~~~

This appears to be a workaround to allow using legacy random number functions in a concurrent environment. 

The System.Random source code is divvied up into pure and monadic functions. 
The _random_ and _randomR_ functions are pure. 

randomR 

:{
rolls :: RandomGen g => Int -> g -> [Word]
rolls n = take n . unfoldr (Just . uniformR (1, 6))
pureGen = mkStdGen 137
:}

:{
rollsM :: StatefulGen g m => Int -> g -> m [Word]
rollsM n = replicateM n . uniformRM (1, 6)
pureGen = mkStdGen 137
:}

, that function is discouraged. Here's the blurb from System.Random:

~~~haskell
Uses the supplied function to get a value from the current global
random generator, and updates the global generator with the new generator
returned by the function. For example, @rollDice@ produces a pseudo-random integer
between 1 and 6:
--
>>> rollDice = getStdRandom (randomR (1, 6))
>>> replicateM 10 (rollDice :: IO Int)
[5,6,6,1,1,6,4,2,4,1]
--
This is an outdated function and it is recommended to switch to its
equivalent 'System.Random.Stateful.applyAtomicGen' instead, possibly with the
'System.Random.Stateful.globalStdGen' if relying on the global state is
acceptable.
--
>>> import System.Random.Stateful
>>> rollDice = applyAtomicGen (uniformR (1, 6)) globalStdGen
>>> replicateM 10 (rollDice :: IO Int)
[4,6,1,1,4,4,3,2,1,2]
~~~

IORef

Here's a discussion from K. A. Buhr about using the IORef data type. It's interesting to read
in light of its use of newIORef in _theStdGen_. 

https://stackoverflow.com/questions/52467957/ioref-in-haskell

Here's a memorable quote:

/quote{
To the extent that mutable state is "bad" and if you really need it better alternatives 
are available depending on whether you do or don't need concurrency, there's not much to recommend IORef.

On the other hand, if you are already working on some non-concurrent code in the IO monad 
because you need to perform actual IO operations, and you genuinely need some pervasive mutable 
state that isn't easy to disentangle from the IO, then using IORefs seems legitimate.
}

This appears to be the main qualifier behind the use of IORef in the System.Random and System.Internal
modules. Snagging system entropy puts you deep into the IO monad and you are warned in 
several places that you should not use the StdGen PRNG if you are concerned with 
concurrency. 

As a note, when K. A. Buhr decides to explain a thing in Haskell, it *always* makes for compelling reading. 
https://stackoverflow.com/users/7203016/k-a-buhr

The comments in System.Random.Stateful gives useful (but cryptic) info on the various
functions that allow using a pure PRNG in monadic containers. 

~~~haskell
Pure pseudo-random number generators can be used in monadic code via the
adapters 'StateGenM', 'AtomicGenM', 'IOGenM', 'STGenM' and 'TGenM'
--
*   'StateGenM' can be used in any state monad. With strict 'StateT' there is
    no performance overhead compared to using the 'RandomGen' instance
    directly. 'StateGenM' is /not/ safe to use in the presence of exceptions
    and concurrency.
--
*   'AtomicGenM' is safe in the presence of exceptions and concurrency since
    it performs all actions atomically.
--
*   'IOGenM' is a wrapper around an 'IORef' that holds a pure generator.
    'IOGenM' is safe in the presence of exceptions, but not concurrency.
--
*   'STGenM' is a wrapper around an 'STRef' that holds a pure generator.
    'STGenM' is safe in the presence of exceptions, but not concurrency.
--
*   'TGenM' is a wrapper around a 'TVar' that holds a pure generator. 'TGenM'
    can be used in a software transactional memory monad 'STM`. It is not as
    performant as 'AtomicGenM`, but it can provide stronger guarantees in a
    concurrent setting.
~~~



So, where am I at? 

I think what I need to do now is to line up all the various ways of getting and using a PRNG
then coming up with my personal strategy for randomization when that comes up in my coding. 

There are a variety of considerations. For one thing, the randomization functions behave 
differently depending on the class of the PRNG. Example:

~~~haskell
--First, a pure PRNG
> pureGen = mkStdGen 42
StdGen {unStdGen = SMGen 9297814886316923340 13679457532755275413}

--Next, a PRNG extracted from IO monad
> fromIOGen <- getStdGen
> fromIOGen
StdGen {unStdGen = SMGen 12865239118265558613 17541939316663907801}

--Next, put an AtomicGen wrapper around a pure PRNG
> atomicGen <- newAtomicGenM pureGen
> applyAtomicGen random atomicGen :: IO Int
1275548033995301424




--Next, monad tricks
--First get a wrapped PRNG
> gen <- newIOGenM pureGen
> :t gen
gen :: IOGenM StdGen

--Pure function and a wrapped PRNG - applyIOGen
> applyIOGen random g :: IO Int
-5352141546736596594

--Monadic functions
> uniformListM 2 gen :: IO [Int]
[-8895873798886633996,-6191778641511186126]
> randomM gen :: IO Double
0.8892180258692887
> randomRM (1, 100) gen :: IO Int
6


--Next, stateful tricks
--Get a pure PRNG
> pureGen = mkStdGen 42

--Run mutable PRNG from a frozen state
--Applying the IOGen type function to the pure StdGen turns it into a monadic PRNG
--The IOGen type is a mutable type
> withMutableGen (IOGen (mkStdGen 217)) (uniformListM 5) :: IO ([Int8], IOGen StdGen)
([48,151,95,15,208],IOGen {unIOGen = StdGen {unStdGen = SMGen 3908126255255093941 13679457532755275413}})

--Using globalStdGen
> replicateM 10 (uniformRM ('a','z') globalStdGen)
"dgvayvtzur"




--Example from Stateful
> withMutableGen (IOGen (mkStdGen 217)) (uniformListM 5) :: IO ([Word8], IOGen StdGen)
([182,37,206,254,3],IOGen {unIOGen = StdGen {unStdGen = SMGen 4273268533320920145 15251669095119325999}})

~~~

When I look at someone else's code and see random number generators in use, my reaction at this 
point is to want to rework them to use currently acceptable idiom. For example, in the 
Haskell Cookbook, the author has a simple blackjack game where he randomizes the shuffle
using this code:

~~~haskell
randomizedList :: [a] -> IO [a]
randomizedList xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
~~~

The intent is to hand this function a list of cards and get back a scrambled (shuffled) deck. 

~~~haskell
--From Main.hs for the blackjack app:
randomDeck =
  randomizedList orderedCardDeck

--from Card.hs
orderedCardDeck :: [Card]
orderedCardDeck = [Card rank suit | rank <- keys rankMap,
                                    suit <- [Hearts .. Spades]]
 |  |  |  |  |  |  |  |  | 
data Suit = Hearts | Diamonds | Clubs | Spades
          deriving (Eq, Show, Enum, Ord)

data Rank = Two | Three | Four
          | Five | Six | Seven | Eight
          | Nine | Ten | Jack  | Queen | King | Ace
          deriving (Eq, Show, Enum, Ord)

rankMap = fromList [(Two,2), (Three,3), (Four,4), (Five,5),
                    (Six,6), (Seven,7), (Eight,8), (Nine,9),
                    (Ten,10), (Jack,10), (Queen,10),
                    (King,10), (Ace,11)]
~~~

So...
He defines two abstract data types and a function that essentially acts as a static map of
card names and values. The _fromList_ function converts this list of tuples into a set of 
values and keys for ranking. 

The orderedCardDeck function uses the integer values from the rankMap list as keys then
assembles a list of rankings in the order specified by the Suit data type. 

This list is then fed to randomizedList. The first thing it does is construct an array using
the contents of the list. This is done because an Array type is mutable. (There must be some
pure way to do this. Will give it some thought.) 

The _forM_ function is interesting. Hadn't encountered it before. It's a flipped version of
_mapM_, which is a way of applying a monadic operation sequentially to each item in a list. 

Example

~~~haskell
> ls = ["those","were","the","days"]
> mapM putStrLn ls
those
were
the
days
[(),(),(),()]

--Use mapM_ to list to skip the empty tuples at the end. 
--Same for forM_, but reverse the list and the function. 
> forM_ ls putStrLn
those
were
the
days
~~~

The Blackjack app walks through a numerical list corresponding to the length of the incoming list
and applies imperative commands in sequence. 

It first creates an ordered pack using the rankmap and the suits. This is formatted as
a standard Haskell list with 52 elements. 

The randomization algo has a nested do block within a do block. The outside do block 
takes the list and uses it to create an Array, more specifically an IO Array. Here's 
where my life got interesting. 

# Array Rathole

The array-0.5.2.0 package defines a collection of mutable and immutable arrays. 
The Data.Array.IO module defines a type called IOArray. This type is a mutable, boxed, non-strict array in the IO monad. 

Non-strict you say? What does that mean? 

The Haskell Wiki (https://wiki.haskell.org/Non-strict_semantics) discusses non-strict semantics. It turns out to 
be one of the cornerstones of the language. To quote:

~~~quote
An expression language is said to have non-strict semantics if expressions can have a value even if some of their 
subexpressions do not. Haskell is one of the few modern languages to have non-strict semantics by default: nearly 
every other language has strict semantics, in which if any subexpression fails to have a value, 
the whole expression fails with it.

This is one of the most important features in Haskell: it is what allows programs to work with conceptually 
infinite data structures, and it is why people say that Haskell lets you write your own control structures. 
It's also one of the motivations behind Haskell being a pure language (though there are several other good ones). 

~~~

Not much help as to what strictness does in practical terms. To the rescue, Manuel M T Chakravarty at the site
TWEAG (https://www.tweag.io/blog/2017-09-27-array-package/). In his third part review of array programming, 
he snags an example from Gentle Intro to Haskell to show why non-strictness is so nice. 

Consider first the code snippet:

~~~haskell
import Data.Array

wavefront :: Int -> Array (Int,Int) Int
wavefront n = arr
  where
    arr = array ((1,1),(n,n))
                ([((1,j), 1) | j <- [1..n]] ++
                 [((i,1), 1) | i <- [2..n]] ++
                 [((i,j), arr!(i,j-1) + arr!(i-1,j-1) + arr!(i-1,j))
                             | i <- [2..n], j <- [2..n]])
~~~

If you feed the _wavefront_ function with a number, it constructs a nxn matrix using the following algo:

Populate the top row with 1's
Populate the left column with 1's
Populate each of the remaining cells with the sum of the cells above, to the left, and diagonally to the left. 

| Column 1 | 

For example:


| 1 | 1 | 1 | 1 | 1 | 
|:----|:---|:----|:----|:----|
|1   |3  |5   |7   |9   | 
|1   |5  |13  |25  |41  |
|1   |7  |25  |63  |129 |

|1   |9  |41  |129 |321 |


Here's what makes this a nice demo of non-strictness. The values for each cell is calculated
by taking values from the cells that have *already been calculated*!! Isn't that cool? 

For example, for cell (3,3) the value is arr!(3,2) + arr!(2,3) + arr!(2,2). Those cells have
already been calculated as 5 + 5 + 3 = 13. 

He goes on to discuss the performance consequences of boxed values. Here's a great quote:

~~~quote
Instead of being able to store those numeric elements in-place in the array, 
non-strict arrays require a boxed representation, where the elements are pointers 
to heap objects containing the numeric values. This additional indirection requires 
extra memory and drastically reduces the efficiency of array access, especially in tight loops. 
~~~

# Exit Rathole

Okay, at this point we have a mutable IO array. 

The business end of the algorithm is in the nested do block. We'll task a variable with collecting whatever returns from the do block. 

Take the stack of things to scramble, such as strings representing Ordinals. 

"One"
"Two"
"Three"
"Four"

Start by considering the entire array of elements, in this case, from element 1 to element 4. 

Randomly select the index location of one of the elements. Let's say index 4. 

Push the value for index 4 ("Four") into the return of the do block. 

Stuff the value for index 1 ("One") into index 4. 

Run the process again but this time start with the second element. (In other words, we leave behind the value we stuffed into index 4.)

Randomly select the index location of one of the elements from 2 to 4. Let's say index 4 again. 

Push the current value for index 4 ("One") into the return of the do block. 

Stuff the value for index 2 ("Two") into index 4. 

Run the process again but this time start with the third element. 

Randomly select either index 3 or 4. Let's say 3. 

Push the current value for index 3 ("Three") into the return of the do block. 

Stuff the value for index 3 ("Three") into index 3. 

Run the process one final time with the final element as the sole range for random selection. Of course the randomizer will select index 4. 

Push the current value for index 4 ("Two") into the return of the do block. 

Stuff the value for index 4 into index 4. 

End of process. 

Take a look at the variable that has been sweeping up after the do block. It will be a list of elements ["Four", "One", "Three", "Two"]. Because they were chosen by a string of random events, the list will be scrambled, and would be scrambled differently at any subsequent run. 

Okay, well, that turns out to be a lot of messing around with a mutable array. Is there some simpler -- that is to say, a more "Haskell-y" way to randomize a list? 

There's a function in System.Random called _randoms_ that generates a random list. But it turns out that 



List of random functions

|1|1|
|--|--|
applyAtomicGen | System.Random.Stateful
applyIOGen | System.Random.Stateful
applyRandomGenM | System.Random.Stateful
applySTGen | System.Random.Stateful
applyTGen | System.Random.Stateful
AtomicGen |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
AtomicGenM |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
Finite | System.Random, System.Random.Stateful
freezeGen | System.Random.Stateful
FrozenGen | System.Random.Stateful
genByteString | System.Random, System.Random.Stateful
genRange | System.Random, System.Random.Stateful
genShortByteString | System.Random, System.Random.Stateful
genShortByteStringIO | System.Random.Stateful
genShortByteStringST | System.Random.Stateful
genWord16 | System.Random, System.Random.Stateful
genWord32 | System.Random, System.Random.Stateful
genWord32R | System.Random, System.Random.Stateful
genWord64 | System.Random, System.Random.Stateful
genWord64R | System.Random, System.Random.Stateful
genWord8 | System.Random, System.Random.Stateful
getStdGen | System.Random, System.Random.Stateful
getStdRandom | System.Random, System.Random.Stateful
globalStdGen | System.Random.Stateful
initStdGen | System.Random, System.Random.Stateful
IOGen |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
IOGenM |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
mkStdGen | System.Random, System.Random.Stateful
MutableGen | System.Random.Stateful
newAtomicGenM | System.Random.Stateful
newIOGenM | System.Random.Stateful
newStdGen | System.Random, System.Random.Stateful
newSTGenM | System.Random.Stateful
newTGenM | System.Random.Stateful
newTGenMIO | System.Random.Stateful
next | System.Random, System.Random.Stateful
Random | System.Random, System.Random.Stateful
random | System.Random, System.Random.Stateful
RandomGen | System.Random, System.Random.Stateful
RandomGenM | System.Random.Stateful
randomIO | System.Random, System.Random.Stateful
randomM | System.Random.Stateful
randomR | System.Random, System.Random.Stateful
randomRIO | System.Random, System.Random.Stateful
randomRM | System.Random.Stateful
randomRs | System.Random, System.Random.Stateful
randoms | System.Random, System.Random.Stateful
runStateGen | System.Random.Stateful
runStateGenST | System.Random.Stateful
runStateGenST_ | System.Random.Stateful
runStateGenT | System.Random.Stateful
runStateGenT_ | System.Random.Stateful
runStateGen_ | System.Random.Stateful
runSTGen | System.Random.Stateful
runSTGen_ | System.Random.Stateful
setStdGen | System.Random, System.Random.Stateful
split | System.Random, System.Random.Stateful
splitGenM | System.Random.Stateful
StatefulGen | System.Random.Stateful
StateGen |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
StateGenM |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
StdGen | System.Random, System.Random.Stateful
STGen |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
STGenM |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
TGen |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
TGenM |  
1 (Type/Class) | System.Random.Stateful
2 (Data Constructor) | System.Random.Stateful
thawGen | System.Random.Stateful
unAtomicGen | System.Random.Stateful
unAtomicGenM | System.Random.Stateful
Uniform | System.Random, System.Random.Stateful
uniform | System.Random, System.Random.Stateful
uniformByteStringM | System.Random.Stateful
uniformDouble01M | System.Random.Stateful
uniformDoublePositive01M | System.Random.Stateful
uniformEnumM | System.Random.Stateful
uniformEnumRM | System.Random.Stateful
uniformFloat01M | System.Random.Stateful
uniformFloatPositive01M | System.Random.Stateful
uniformListM | System.Random.Stateful
uniformM | System.Random.Stateful
uniformR | System.Random, System.Random.Stateful
UniformRange | System.Random, System.Random.Stateful
uniformRM | System.Random.Stateful
uniformShortByteString | System.Random.Stateful
uniformViaFiniteM | System.Random.Stateful
uniformWord16 | System.Random.Stateful
uniformWord32 | System.Random.Stateful
uniformWord32R | System.Random.Stateful
uniformWord64 | System.Random.Stateful
uniformWord64R | System.Random.Stateful
uniformWord8 | System.Random.Stateful
unIOGen | System.Random.Stateful
unIOGenM | System.Random.Stateful
unStateGen | System.Random.Stateful
unSTGen | System.Random.Stateful
unSTGenM | System.Random.Stateful
unTGen | System.Random.Stateful
unTGenM | System.Random.Stateful
withMutableGen | System.Random.Stateful
withMutableGen_ | System.Random.Stateful

