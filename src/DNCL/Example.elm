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
    """Ribon[1] ← 55
Ribon[2] ← 53
Ribon[3] ← 31
Ribon[4] ← 37
Ribon[5] ← 37
Ribon[6] ← 22
Ribon[7] ← 13
Ribon[8] ← 19
Ribon[9] ← 25
Ribon[10] ← 16
Ribon[11] ← 0


「---- 問1 ----」を表示する

tokuten ← 0
takasa ← 55

i を 1 から 11 まで 1 ずつ増やしながら，
    もし Ribon[i] ≧ takasa － 5 かつ Ribon[i] ≦ takasa ならば
        tokuten ← tokuten ＋ 1
    を実行する
    takasa ← takasa － 5
を繰り返す

「得点は」と tokuten と「点」を表示する


「---- 問2 ----」を表示する

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


「---- 問3 ----」を表示する

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

「獲得可能な最高得点は」と Kokomade[11] と「点」を表示する

"""


pastExam2022 : SourceCode
pastExam2022 =
    """「---- 問1 ----」を表示する

tate ← 3
yoko ← 4
Yokosen[1] ← 2
Yokosen[2] ← 1
Yokosen[3] ← 2
Yokosen[4] ← 1

y を 1 から yoko まで 1 ずつ増やしながら，
    x ← 1
    x ≦ tate の間，
        もし Yokosen[y] ＝ x ならば
            「┣」を改行なしで表示する
            「┫」を改行なしで表示する
            x ← x ＋ 2
        を実行し，そうでなければ
            「┃」を改行なしで表示する
            x ← x ＋ 1
        を実行する
    を繰り返す
    改行を表示する
を繰り返す


「---- 問2 ----」を表示する

関数 配列を表示する(Koma) を
    j を 1 から 要素数(Koma) まで 1 ずつ増やしながら，
        Koma[j] を改行なしで表示する
    を繰り返す
    改行を表示する
と定義する

関数 あみだくじを表示する(tate， Yokosen， yoko) を
    y を 1 から yoko まで 1 ずつ増やしながら，
        x ← 1
        x ≦ tate の間，
            もし Yokosen[y] ＝ x ならば
                「┣」を改行なしで表示する
                「┫」を改行なしで表示する
                x ← x ＋ 2
            を実行し，そうでなければ
                「┃」を改行なしで表示する
                x ← x ＋ 1
            を実行する
        を繰り返す
        改行を表示する
    を繰り返す
と定義する


Koma[1] ← 1
Koma[2] ← 2
Koma[3] ← 3

配列を表示する(Koma)
あみだくじを表示する(要素数(Koma)， Yokosen， 要素数(Yokosen))

y を 1 から 要素数(Yokosen) まで 1 ずつ増やしながら，
    t ← Koma[Yokosen[y]]
    Koma[Yokosen[y]] ← Koma[Yokosen[y] ＋ 1]
    Koma[Yokosen[y] ＋ 1] ← t
を繰り返す

配列を表示する(Koma)


「---- 問3 ----」を表示する

Koma[1] ← 5
Koma[2] ← 2
Koma[3] ← 4
Koma[4] ← 3
Koma[5] ← 1

配列を表示する(Koma)

yoko ← 0
p を 1 から 要素数(Koma) － 1 まで 1 ずつ増やしながら，
    q を 1 から 要素数(Koma) － p まで 1 ずつ増やしながら，
        もし Koma[q] ＞ Koma[q ＋ 1] ならば
            t ← Koma[q]
            Koma[q] ← Koma[q ＋ 1]
            Koma[q ＋ 1] ← t
            yoko ← yoko ＋ 1
            Yokosen[yoko] ← q
        を実行する
        "p = " と p と ", q = " と q と ", Koma = " を改行なしで表示する
        配列を表示する(Koma)
    を繰り返す
を繰り返す

あみだくじを表示する(要素数(Koma)， Yokosen， yoko)
配列を表示する(Koma)

"""
