import Data.List
import Data.Char
-- 順番
data Turn = Me | You deriving(Eq)
-- ○×マーク
-- 先攻なら×、後攻なら○
data Mark = X | O | Empty deriving(Eq)
instance Show Mark where
    show X = "X"
    show O = "O"
    show _ = " "
-- 座標
type Point = (Int, Int)
-- ゲーム盤
type Board = [(Point, Mark)]
-- ゲーム盤の大きさ
-- TODO: 使ってない…
board_len :: (Int, Int)
board_len = (3, 3)
-- ゲームの歴史
-- TODO: 定義
type History = Board
history = []
-- ゲームパラメータ
type Param = (Turn, Board, History)
-- ゲーム盤を描く
-- TODO: 綺麗に短くしたい
write_board :: Board -> IO Board
write_board b = do putStrLn " -A-B-C-"
                   putStr "1|"
                   putStr $ (show $ get_mark b (1, 1)) ++ "|"
                   putStr $ (show $ get_mark b (2, 1)) ++ "|"
                   putStrLn $ (show $ get_mark b (3, 1)) ++ "|"
                   putStrLn " -------"
                   putStr "2|"
                   putStr $ (show $ get_mark b (1, 2)) ++ "|"
                   putStr $ (show $ get_mark b (2, 2)) ++ "|"
                   putStrLn $ (show $ get_mark b (3, 2)) ++ "|"
                   putStrLn " -------"
                   putStr "3|"
                   putStr $ (show $ get_mark b (1, 3)) ++ "|"
                   putStr $ (show $ get_mark b (2, 3)) ++ "|"
                   putStrLn $ (show $ get_mark b (3, 3)) ++ "|"
                   putStrLn " -------"
                   return b
-- 座標からそこにあるマークを取得
get_mark :: Board -> Point -> Mark
get_mark b p = case find (\(x, y) -> x == p) b of
                    Just (p, m) -> m
                    _           -> Empty
-- コンピュータが打つ場所決める
-- TODO: ちゃんと実装。今は順番に置いてるだけ
put_next_mark :: Param -> Board
put_next_mark (t, b, h) = case c of
                               (x:xs) -> (x, mark) : b
    where a = [(x, y) | x <- [1..3], y <- [1..3]]
          b2 = map (\((x, y), z) -> (x, y)) b
          c = a \\ b2
          mark = if odd $ length b then O else X
-- ユーザが打つ場所入力する
-- TODO: エラーチェック
input_next_mark :: Param -> IO Board
input_next_mark (t, b, h) = do putStrLn "input?[A1-C3]:"
                               str <- getLine
                               case str of
                                    (x:y:[]) -> return $ (convert x y, mark) : b
                                    _        -> input_next_mark (t, b, h)
                            where -- 先攻なら×、後攻なら○
                                  mark = if odd $ length b then O else X
                                  -- 入力を座標に変換
                                  convert x y = (ord (toUpper x) - ord 'A' + 1, read [y] :: Int)
-- 勝敗が決まったか？
-- TODO: 綺麗に短くしたい
is_win :: Board -> Bool
is_win b = if length (intersect b [((x, 1), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((x, 2), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((x, 3), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((1, x), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((2, x), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((3, x), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((x, x), mark) | x <- [1..3]]) == 3 ||
              length (intersect b [((x, 4 - x), mark) | x <- [1..3]]) == 3
              then True
              else False
           where mark = if odd $ length b then X else O
-- ゲーム開始
main :: IO ()
main = gameStart (Me, [], h)
       where -- ゲームの歴史の初期化
             h = []
-- ゲーム開始
gameStart :: Param -> IO ()
gameStart (_, _, h) = do -- ゲーム開始
                         putStrLn "game start !!"
                         -- 先手・後手を決める
                         -- TODO: 先手・後手を決める
                         t <- return Me
                         -- 開始
                         write_board b
                         (t2, b2, h2) <- game (t, b, h)
                         case is_win b2 of
                            True -> if t2 == Me then print "i'm winner" else print "you're winner"
                         -- TODO: 歴史に今回のゲームを追加
                         h3 <- return h2
                         gameStart (t2, b2, h3)
                      where -- ゲーム盤の初期化
                            b = []
-- 一手打つ
game :: Param -> IO (Param)
-- コンピュータの番
game (Me, b, h)  = do b3 <- write_board b2
                      if is_win b3 then return (Me, b3, h)
                                   else game (You, b3, h)
                   where b2 = put_next_mark (Me, b, h)
-- ユーザの番
game (You, b, h) = do b2 <- input_next_mark (You, b, h)
                      b3 <- write_board b2
                      if is_win b3 then return (You, b3, h)
                                   else game (Me, b3, h)
