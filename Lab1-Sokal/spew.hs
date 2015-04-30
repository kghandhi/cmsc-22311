import System.Random
import System.IO
import Data.Array
import Data.List
import Control.Monad.State.Lazy
import System.Environment (getArgs)

type FastModel = Array Int (String, [(Int, Int)])
type RandState = State StdGen

feed :: String -> FastModel
feed raw = listArray (0, (length processModel) - 1) processModel
  where processModel = map read $ lines raw

weighter :: [(Int, Int)] -> Int -> Int
weighter succs rand = wait succs 0
  where
    wait ((weight, st):rest) acc
      | acc + weight >= rand = st
      | otherwise = wait rest (acc + weight)
    wait [] acc = -1

select :: [(Int, Int)] -> RandState Int
select succs = fmap (weighter succs) $ state $ randomR (0, sum $ map (fst) succs)

runModel :: FastModel -> RandState [String]
runModel fm = do
  start <- state.randomR $ bounds fm
  iter start where
    iter idx = do
      let (word, succs) = fm ! idx
      nxt <- select succs
      case inRange (bounds fm) nxt of
       False -> fmap (word:) $ (state . randomR) (bounds fm) >>= (iter)
       True -> fmap (word:) $ iter nxt

isTerminator :: String -> Bool
isTerminator s = any (== last s) ['.', '?', '!']

takeEnough :: Int -> [String] -> [String]
takeEnough need ws = map fst $ takeWhile' notDone $ zip ws [1..]
  where
    notDone (w, i) = (i < need) || ((i >= need) && ((not . isTerminator) w))
    takeWhile' _ [] = []
    takeWhile' p (x:xs)
      | p x = x:(takeWhile' p xs)
      | otherwise = [x]

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"

getSpewSize :: [String] -> Int
getSpewSize [] = 10
getSpewSize (x:xs) = read x

main = do
  args <- getArgs
  let spewSize = getSpewSize args
  raw <- readFile "sokal.model"
  let model = feed raw
  gen <- getStdGen
  let ws = evalState (runModel model) gen
  putStr $ linefill 72 $ takeEnough spewSize ws

{-
Output:
$ ./spew 0
> Mexico's former Secretary of War, Chaos, and History, criticizes similar
big picture, rigid command-control-communications models that create
use-value in circulation with and displaces the suffering of the traces
of the linkages between fascism and can we avoid seeing in the narrative
using stories or plots.

$ ./spew 10
> my intuition or representation of the black man from an analogy between
the conceptual as spontaneous and self-contained, come to Lvi-Strauss
after reading this essay): a picture of reality-in-print.

$ ./spew
> compelled to 'space the void'" ("Wearable Space" 369-370). The strongest
confirmation of prior experience.

$ ./spew 100
I find these crossed arguments particularly interesting now that we
always wanted to catechize the full text as merely culturally mediated
forms of embodiment and activities. It takes a different form around
questions of self-governance, governance, and subjection. That the
protagonist has this specific form of an epoch, and an entitlement that
is small enough to get out of the qasidah, architectural concepts are
often associated with Rastafarian style (green, yellow, red) and two
ecstatic drag queens in an interminable spiritual process of accepting
products without understanding how cultures work. Following the
unidirectionality of death, reproduces itself in a open and "political"
manner, the transition to world capitalism, a figure for the very
possibility of their bearers themselves)34 ; but he is "prepared to find
that shopping has provided the clout needed to be guarded.

$ ./spew 1000
> Objects Stationed in The Man in the villages where they released the
single spotlight; unfurnished, except for the production of interiority
visually captured, the penetrated interior appears as the famous,
antique sculpture of Lacoon and his (and our) own. So as to whether or
not understanding may be the buyers themselves who are some of that need
a psycho-sexual Kinsey Index correlates with a deadening sense of the
loss of function--in other words the Drivotrainer cannot correct
perception, no matter how innocent or guilty they may be on both the
online child, alternately monster and victim now all-too familiar from
photographic imagery and other culture sites. Theres no way succumbing
to its avowed goal of social art itself; the expertise of an ongoing
contribution to the sciences of life to the sixth edition of Immanuel
Kant's Critique of Cynical Reason. We may adapt this image of a visual
artist, is well defined, the relationship between the technical material
is the expression may sound to classical Hollywood model, filmmakers can
boost their own devices . . is a profound change in Barcelona and
Catalua as Xarnegos, to the coexistence of this hermeneutic of twisted,
internally inadequate concepts is Althusser's reading of the sovereign?
Or does it mean for time to come, with which "the bourgeoisie idealizes
and universalizes (today we might reject the attribution of lapses in
memory to some hotel on the last letter or syllable of a text's
self-enclosure, its formal indifference or even madness in the McGraw
Hill definition supports my aim to provide access for all others. The
adding is therefore not a world within a wider exploration of a notion
of organization and order enforced, if proper controls are imposed upon
us a clue as causative variable, Moretti assures us of "a pathology of
cultural and political mastery enables the intimacy of the Mmorial des
enfants, Dora Bruder is an enactment, and simultaneously under the
buzzing comfort of closure, she seeks to reduce social inequalities
especially if a relative geopolitical and ecopolitical), but also the
intangible Internet generates the movie set as though we are introduced
to African, Caribbean, Spanish, and Latin America was, like today, up
for the origins of twentieth-century French thought: must social
movements from the benevolence of the development of new regimes of
subjection, of the loss of the soldier, the image, or the other hand,
she is not entirely constrained by either side by side with the lake
water to problematize any strict distinction between "things" and
"objects," he fails the trick and ride out nuclear exchange. He faces
the viewer can then use to imagine another kind of rent based on the
forum, was delegated to go where I see this mirroring, even when it is
the main tenor of this labor power which I have a suspicion, of course,
subjected them to what he would be available for wholesale
transformation, he does not know, and what he meant nature. On this
point, it is first approached by the great contemporary fantasy that
passivity equals femininity also made him realize that he has been posed
in a potential refugee, ultimately displacing onto capital all the
individuals internal master at the same time, memories that constitute
it. Thus, Balibar locates in Marx (which, in capitalism, usurps the
place of Marx and Lacan, or of one of the conjunction of Freud and
Benjamin, conceives of it and find out where they have kept up between
Patty, Bill, and their institutions against loss. They, like Baudelaires
narrator, beat up the sexual characterization of the kind of false
starts, interruptions, and failures. Overall, the decay of production,
we also have an impact on its head. As Johnson points out, the diphones
suggest that Lovecraft "attempted to pronounce sorcery's final word" (TP
251; sec. 10), although Lovecraft has received from God to humanlike
intelligence" (55). Yet in what follows, by contrast, appeals to the
aesthetic body (182). In this way, a continued provocation of "anxieties
that surround changing definitions and axioms to theorems by means of
regulating life in the suspension of disbelief that takes into account
everyone elses feelings and sensitivities). Putting oneself in life, and
it is an unstable and at the same time ecstatic and intolerable.'"
(Regarding 98) Before delving further into this tradition by reasserting
the generic turf of what suggests, perhaps even life, from culture to
produce a new height of the economic and political life (Dits 15541565).
His idealization of sadomasochism serves to alleviate the seeping blood
all serve to complicate matters further, "myself") in this world. And
yet, the subject in a more experimental criticism, seem rather
non-academic; however, Cage's turn to Balibar's "Citizen Subject," and
Roberto Esposito briefly describes this semantic space," then one that
relies upon the heteroglossic potential of all as a general idea that
art crosses the divide between the "subjective" space of truth, irony is
often overlooked how explicitly political the U.S. reveal the
negotiations of authorial and other insurance optionsin other words they
are pre-philosophical. Werewolves, demons, and vampires are single
expressions of his homeland. The characters too, with Bleeding Edge, he
presents as the primary means for addressing environmental degradation.
However, the type considered here, has been the subject itself. I quote
at length on Haussmann's renovation of Paris, which sought to damn their
professionally-trained brethren while re-classifying themselves as
bastards, it is a home for the subject. Sex is sex, but he is a question
rather than mutating technology (see Fig. 9 above). While Coder and
Runner, the Tokyo Rose appear through roving screens that promise
nothing beyond the usual dichotomy dividing moral democracy from immoral
tyranny. This is almost entirely a found text of YOUThe City becomes
proportional to the world, while at the very impulse to another of duty,
a remorseless guilt, eating its way into thinking Steorn had discovered
a bas-relief which was occurring around her and used her as an event
beyond attribution, occurring in the problems of contemporary society,
benefit from its composer, the closing of a community in the form of
dispensing entirely with the gift.
-}
