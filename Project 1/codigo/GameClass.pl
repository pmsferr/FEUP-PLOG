%==================%
%= @@ game object =%
%==================%

%%% creates a game "object".
%%% 1. game, a list with:
%%%  0. the board state;
%%%  1. a list with: the number of pieces of each player;
%%%  2. the current player turn;
%%%  3. the game mode: player vs. player (pvp)
createPvPGame(Game):-
	initializeBoard(Board),
    Game = [Board, [32, 32], whitePlayer, pvp], !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decNumWhitePieces(Game, ResultantGame):-
	getGameNumWhitePieces(Game, NumWhitePieces),
	NumWhitePieces1 is NumWhitePieces - 1,
	setGameNumWhitePieces(NumWhitePieces1, Game, ResultantGame).

decNumBlackPieces(Game, ResultantGame):-
	getGameNumBlackPieces(Game, NumBlackPieces),
	NumBlackPieces1 is NumBlackPieces - 1,
	setGameNumBlackPieces(NumBlackPieces1, Game, ResultantGame).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% gets the board of the game specified.
%%% 1. game; 2. board of game.
getGameBoard([Board|_], Board).

%%% sets the board of the game specified.
%%% 1. board to be set; 2. current game; 3. resultant game with the new board.
setGameBoard(Board, Game, ResultantGame):-
	setListElemAtWith(0, Board, Game, ResultantGame).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getGameNumPiecesList(Game, NumPiecesList):-
	getListElemAt(1, Game, NumPiecesList).

setGameNumPiecesList(NumPiecesList, Game, ResultantGame):-
	setListElemAtWith(1, NumPiecesList, Game, ResultantGame).

getGameNumWhitePieces(Game, NumWhitePieces):-
	getGameNumPiecesList(Game, NumPiecesList),
	getListElemAt(0, NumPiecesList, NumWhitePieces).

setGameNumWhitePieces(NumWhitePieces, Game, ResultantGame):-
	getGameNumPiecesList(Game, NumPiecesList),
	setListElemAtWith(0, NumWhitePieces, NumPiecesList, ResNumPiecesList),
	setGameNumPiecesList(ResNumPiecesList, Game, ResultantGame).

getGameNumBlackPieces(Game, NumBlackPieces):-
	getGameNumPiecesList(Game, NumPiecesList),
	getListElemAt(1, NumPiecesList, NumBlackPieces).

setGameNumBlackPieces(NumBlackPieces, Game, ResultantGame):-
	getGameNumPiecesList(Game, NumPiecesList),
	setListElemAtWith(1, NumBlackPieces, NumPiecesList, ResNumPiecesList),
	setGameNumPiecesList(ResNumPiecesList, Game, ResultantGame).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getGamePlayerTurn(Game, Player):-
	getListElemAt(2, Game, Player).

setGamePlayerTurn(Player, Game, ResultantGame):-
	setListElemAtWith(2, Player, Game, ResultantGame).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getGameMode(Game, Mode):-
	getListElemAt(3, Game, Mode).

setGameMode(Mode, Game, ResultantGame):-
setListElemAtWith(3, Mode, Game, ResultantGame).