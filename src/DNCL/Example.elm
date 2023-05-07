module DNCL.Example exposing (..)

import DNCL.AST exposing (SourceCode)


helloWorld : SourceCode
helloWorld =
    "「こんにちは、世界」を表示する"


euclid : SourceCode
euclid =
    """x ← 1071
y ← 1029

copy_x ← x
copy_y ← y

y ≠ 0 の間，
    "x = " と x と ", y = " と y を表示する
    tmp ← y
    y ← x ％ y
    x ← tmp
を繰り返す

"gcd(" と copy_x と ", " と copy_y と ") = " と x を表示する"""


bubbleSort : SourceCode
bubbleSort =
    """Arr ← {7， 2， 1， 4， 0， 5， 6， 3}

i を 要素数(Arr) － 2 から 0 まで 1 ずつ減らしながら，
    j を 0 から i まで 1 ずつ増やしながら，
        もし Arr[j] ＞ Arr[j ＋ 1] ならば
            "Arr = " と Arr を表示する
            tmp ← Arr[j]
            Arr[j] ← Arr[j ＋ 1]
            Arr[j ＋ 1] ← tmp
        を実行する
    を繰り返す
を繰り返す

"Arr = " と Arr を表示する"""


pastExam2023 : SourceCode
pastExam2023 =
    """Ribon ← {-1， 55， 53， 31， 37， 37， 22， 13， 19， 25， 16， 0}


「問1」を表示する

tokuten ← 0
takasa ← 55

i を 1 から 11 まで 1 ずつ増やしながら，
    もし Ribon[i] ≧ takasa － 5 かつ Ribon[i] ≦ takasa ならば
        tokuten ← tokuten ＋ 1
    を実行する
    takasa ← takasa － 5
を繰り返す

「得点は」と tokuten と「点」を表示する


「問2」を表示する

tokuten ← 0
takasa ← 55

i を 1 から 11 まで 1 ずつ増やしながら，
    もし Ribon[i] ≦ takasa ならば
        tokuten ← tokuten ＋ 1
        takasa ← Ribon[i]
    を実行する
を繰り返す

「得点は」と tokuten と「点」を表示する

GENDO ← 20
tokuten ← 0
takasa ← 55

i を 1 から 11 まで 1 ずつ増やしながら，
    もし (Ribon[i] ≦ takasa かつ takasa － Ribon[i] ＜ GENDO) または i ＝ 11 ならば
        tokuten ← tokuten ＋ 1
        takasa ← Ribon[i]
    を実行する
を繰り返す

「得点は」と tokuten と「点」を表示する


「問3」を表示する

Kokomade ← {-1， 0， 0， 0， 0， 0， 0， 0， 0， 0， 0， 0}
Kokomade[1] ← 1

i を 2 から 11 まで 1 ずつ増やしながら，
    saikou ← 0
    t を 1 から i － 1 まで 1 ずつ増やしながら，
        もし Ribon[t] ≧ Ribon[i] かつ saikou ＜ Kokomade[t] ならば
            saikou ← Kokomade[t]
        を実行する
    を繰り返す
    Kokomade[i] ← saikou ＋ 1
を繰り返す

「獲得可能な最高得点は」と Kokomade[11] と「点」を表示する"""
