-module(test).
-include("c:/src/erlang/esdl/include/sdl.hrl").
-include("c:/src/erlang/esdl/include/sdl_events.hrl").
-include("c:/src/erlang/esdl/include/sdl_audio.hrl").
-include("c:/src/erlang/esdl/include/sdl_video.hrl").
-include("c:/src/erlang/esdl/include/sdl_keyboard.hrl").
-include("c:/src/erlang/esdl/src/sdl_util.hrl").
-include("c:/src/erlang/esdl/include/gl.hrl").
-export([go/0, go/1]).

-define(W, 640).
-define(H, 480).
-define(NumCols, 10).
-define(NumRows, 10).
-define(NumMines, 10).
-define(MaxY, 480).
-define(MaxX, 640).

-record(col, {seen = false, mined = false, marked = false, numAround = 0}).

setupBoard() ->
    setupBoard(?NumCols, ?NumRows, []).

setupBoard(Width, Height, Board) ->
    if Height /= 0 ->
           setupBoard(Width, Height - 1, [setupRow(Width)|Board]);
       true ->
           Board
    end.

setupRow(Width) ->
    setupRow(Width, []).

setupRow(Width, Row) ->
    if Width /= 0 ->
           setupRow(Width - 1, [#col{} | Row]);
       true ->
           Row
    end.

pprintBoard([]) ->
    io:format("~n");

pprintBoard(Board) ->
    [Row|Rest] = Board,
    Fun = fun(Col) ->
        if
            Col#col.mined == true ->
                io:format("* ");
            true ->
                io:format("~p ", [Col#col.numAround])
        end
    end,
    lists:map(Fun, Row),
    io:format("~n"),
    pprintBoard(Rest).

loopOverBoard(ColumnFun, Board) ->
    ProcessRow = fun({Y, Row}) ->
        Fn = fun ({X, Col}) ->
            ColumnFun({X, Y, Col})
        end,
        lists:map(Fn, lists:zip(lists:seq(0, 9), Row))
    end,
    lists:map(ProcessRow, lists:zip(lists:seq(0, 9), Board)).

around(Fn, X, Y, Board) ->
    Around = [
        {(X + 1), (Y + 1)},
        {(X + 1), (Y)},
        {(X + 1), (Y - 1)},
        {(X),     (Y + 1)},
        {(X),     (Y + 1)},
        {(X),     (Y + 1)},
        {(X - 1), (Y + 1)},
        {(X - 1), (Y)},
        {(X - 1), (Y - 1)}],
    AroundFn = fun({Col, Row}) ->
        if (Col >= 0) and (Row >= 0) and (Col < 10) and (Row < 10) ->
            Fn({Col, Row, Board});
        true ->
            Board
        end
    end.

getNumberAround(Fn, X, Y, Board) ->
    Around = [
        {(X + 1), (Y + 1)},
        {(X + 1), (Y)},
        {(X + 1), (Y - 1)},
        {(X),     (Y + 1)},
        {(X),     (Y + 1)},
        {(X),     (Y + 1)},
        {(X - 1), (Y + 1)},
        {(X - 1), (Y)},
        {(X - 1), (Y - 1)}],
    AroundFn = fun({Col, Row}) ->
        if (Col >= 0) and (Row >= 0) and (Col < 10) and (Row < 10) ->
            Fn({Col, Row, Board});
        true ->
            0
        end
    end,
    lists:sum(lists:map(AroundFn, Around)).

isMined(X, Y, Board) ->
    Col=lists:nth(X+1, lists:nth(Y+1, Board)),
    Col#col.mined.

getCell(X, Y, Board) ->
    lists:nth(X+1, lists:nth(Y+1, Board)).

addNumberAround(Board) ->
    Around = fun({X, Y, Board}) ->
        Mined = isMined(X, Y, Board),
        if Mined -> 1; true -> 0 end
    end,
    ColumnFun = fun({X, Y, Col}) ->
        Col#col{numAround = getNumberAround(Around, X, Y, Board)}
    end,
    loopOverBoard(ColumnFun, Board). 

clearCell(X, Y, Board) ->
    NewBoard = loopOverBoard(
        fun({C, R, Col}) ->
            if (X == C) and (Y == R) ->
                Col#col{seen = true};
            true -> Col end end, Board),
    around(fun({X, Y, Board}) -> Col = getCell(X, Y, Board), if Col#col.numAround == 0 -> clearCell(X, Y, Board); true-> Board end end, X, Y, Board).

markCell(X, Y, Board) ->
    loopOverBoard(fun({C, R, Col}) -> if (X == C) and (Y == R) -> Col#col{marked = true}; true -> Col end end, Board).

setMine(X, Y, Board) ->
    loopOverBoard(fun({C, R, Col}) -> if (X == C) and (Y == R) -> Col#col{mined = true}; true -> Col end end, Board).


createAndMineBoard() ->
    Board = setupBoard(),
    {Seed1, Seed2, Seed3} = now(),
    %State = random:seed(Seed1, Seed2, Seed3),
    State = random:seed0(),
    MinedBoard = mineBoard(State, Board),
    addNumberAround(MinedBoard).

mineBoard(OldState, Board) ->
    NumMines = numberOfMinesLaid(Board),
    if
        NumMines /= 10 ->
            {X, State} = random:uniform_s(10, OldState),
            {Y, NewState} = random:uniform_s(10, State),
            mineBoard(NewState, setMine(X, Y, Board));
        true ->
            Board
    end.

numberOfMinesLaid(Board) ->
    ProcessRow = fun({_, Row}) ->
        ProcessCol = fun({_, Col}) ->
            if Col#col.mined ->
                    1;
               true ->
                    0
            end
        end,
        lists:sum(lists:map(ProcessCol, lists:zip(lists:seq(0, 9), Row)))
    end,
    lists:sum(lists:map(ProcessRow, lists:zip(lists:seq(0, 9), Board))).

go() ->
    go([window]).

go(Mode) ->
    Wrapper = sdl:init(?SDL_INIT_VIDEO bor ?SDL_INIT_NOPARACHUTE),
    %sdl_util:debug(5),
    sdl_events:eventState(?SDL_ALLEVENTS, ?SDL_IGNORE),
    sdl_events:eventState(?SDL_MOUSEBUTTONDOWN, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_MOUSEBUTTONUP, ?SDL_ENABLE),
    %sdl_events:eventState(?SDL_VIDEOEXPOSE, ?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT, ?SDL_ENABLE),
    Flags =
    case lists:member(fullscreen, Mode) of
        true ->
        ?SDL_OPENGL bor ?SDL_FULLSCREEN;
        _ ->
        ?SDL_OPENGL
    end,
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),
    Screen = sdl_video:setVideoMode(?W, ?H, 16, Flags),
    sdl_video:wm_setCaption("Erlang Minesweeper", "Erlang Minesweeper"),
    Board = createAndMineBoard(),
    pprintBoard(Board),
    check_event(Board),
    sdl:quit(),
    ok.

coord(Coord, Type) ->
    case Type of
        x -> NewCoord = ((Coord / 5.0) - 1.0);
        y -> NewCoord = (1.0 - (Coord / 5.0))
    end,
    NewCoord.

drawLine(X1, Y1, X2, Y2) ->
    gl:color3f(1.0, 1.0, 1.0),
    gl:glBegin(?GL_LINES),
    gl:vertex2d(coord(X1, x), coord(Y1, y)),
    gl:vertex2d(coord(X2, x), coord(Y2, y)),
    gl:glEnd().
    
drawBox({R, G, B}, X, Y) ->
    gl:color3f(R, G, B),
    gl:glBegin(?GL_QUADS),
    gl:vertex2d(coord(X, x), coord(Y, y)),
    gl:vertex2d(coord(X, x), coord(Y+1, y)),
    gl:vertex2d(coord(X+1, x), coord(Y+1, y)),
    gl:vertex2d(coord(X+1, x), coord(Y, y)),
    gl:glEnd().

display(Board) ->
    gl:clearColor(0.0, 0.0, 0.0, 0.0),
    gl:clear(?GL_COLOR_BUFFER_BIT),
    ColumnFun = fun({X, Y, Col}) ->
        if Col#col.seen and Col#col.mined ->
            drawBox({1.0, 0.0, 0.0}, X, Y),
            ok; %Draw red box.
           Col#col.marked ->
            drawBox({1.0, 1.0, 0.0}, X, Y),
            ok; %Draw yellow box.
           Col#col.seen == true ->
            drawBox({0.5, 0.5, 0.5}, X, Y),
            ok; %Draw gray box.
           true ->
            drawBox({0.0, 0.0, 0.0}, X, Y),
            ok %Draw black box.
        end
    end,
    loopOverBoard(ColumnFun, Board),
    lists:map(fun(X) -> drawLine(X, 0, X, ?MaxY) end, lists:seq(0, 10)),
    lists:map(fun(Y) -> drawLine(0, Y, ?MaxX, Y) end, lists:seq(0, 10)),
    gl:swapBuffers().

check_event(Board) ->
    display(Board),
    case sdl_events:pollEvent() of
        #quit{} ->
            true;
        no_event ->
            timer:sleep(100),
            %io:format("no event.~n"),
            check_event(Board);
        Event ->
            case Event of
                #expose{} ->
                    display(Board);
                #mousebutton{} ->
                    {_, _, Button, Pressed, Mods, X, Y} = Event,
                    Col = X div (?W div ?NumCols),
                    Row = Y div (?H div ?NumRows),
                    if (Button == 1) ->
                        NewBoard = clearCell(Col, Row, Board);
                       (Button == 3) ->
                        NewBoard = markCell(Col, Row, Board);
                       true ->
                        NewBoard = Board
                    end,
                    io:format("~b~n", [Col]),
                    io:format("~p~n", [Event]),
                    io:format("mouse button pressed.~n"),
                    timer:sleep(100),
                    check_event(NewBoard);
                _ ->
                    io:format("Received event ~p~n", [Event]),
                    timer:sleep(100),
                    check_event(Board)
            end
    end.
