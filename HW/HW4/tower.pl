% Question 1:
% ntower/3 with arguments:
%   N, a nonnegative integer specifying the size of the square grid.
%   T, a list of N lists, each representing a row of the square grid.
%       Each row is represented by a list of N distinct integers from 1 through N.
%       The corresponding columns also contain all the integers from 1 through N.
%   C, a structure with function symbol counts and arity 4.
%       Its arguments are all lists of N integers, and represent the tower counts for the top,
%       bottom, left, and right edges, respectively.

% base cases: fact_1: 0x0 grid, which contains nothing, so counts lists has nothing
%             fact_2: 1x1 grid, which can only contain a single 1, so counts lists all has a singleton 1 
ntower(0, [], counts([],[],[],[])).
ntower(1, [[1]], counts([1],[1],[1],[1])).

% inductions:
ntower(N, T, C) :-
    % checking argument: N has to be greater than 1
    N > 1,
    % checking argument: C is a structure holding 4 lists, each representing Top, Bottom, Left, Right
    C = counts(Top, Bottom, Left, Right),
    % checking length of row in T is N (row length)
    check_row_length(T, N),
    % checking length of each row in T is also N (column length)
    check_col_length(T, N),
    % checking each row of T is in domain [1, N]
    check_domain(T, N),
    % checking each row of T has no duplicates
    check_no_dup(T),
    % to check rows, we transpose T to Ttrans, so each row in Ttrans corresponds to original colum in T
    transpose(T, Ttrans),
    check_domain(Ttrans, N),
    check_no_dup(Ttrans),
    maplist(fd_labeling, T),

    % now we want to check the number of visible towers, and there are four perspectives:
    % Top, Bottom, Left, Right. The idea here is to transform T into four matrix, and
    % then count the numbers from left to right (row-wise). T is already good for Left,
    % and transpose of T is good for Top. Then based on these two matrix, we need a horizontal
    % flipping method to generate the other two matrics for Right and Bottom.
    horizontal_flip(T, Tright),
    horizontal_flip(Ttrans, Tbottom),

    % then we can check the number of visible tower
    check_count(T, Left),
    check_count(Ttrans, Top),
    check_count(Tright, Right),
    check_count(Tbottom, Bottom).


% auxiliary function to check the number of rows in T is N
check_row_length(T, N) :-
    length(T, N).

% auxiliary function to check the number of columns in T is N
check_col_length([], _).
check_col_length([HD|TL], N) :-
    length(HD, N),
    check_col_length(TL, N).

% auxiliary function to check the domain of each row
check_domain([], _).
check_domain([HD|TL], N) :-
    fd_domain(HD, 1, N),
    check_domain(TL, N).

% auxiliary function to check there is no duplicates in each row
check_no_dup([]).
check_no_dup([HD|TL]):-
    fd_all_different(HD),
    check_no_dup(TL).

% auxiliary function to transpose a matrix
transpose([], []).
% if the first row is empty, then all rows are empty, as we checked the matrix is N*N
transpose([[]|_], []).
transpose(Matrix, [Row|RestRow]) :-
    first_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, RestRow).

% auxiliary function for transpose
% first_column/3 extracts the first column of the matrix
first_column([], [], []).
first_column([[H|T]|RestRow], [H|Hs], [T|Ts]) :-
    first_column(RestRow, Hs, Ts).

% auxiliary function for horizontal_flip
horizontal_flip([], []).
horizontal_flip([HD_origin|TL_origin], [HD|TL]) :-
    reverse(HD_origin, HD),
    horizontal_flip(TL_origin, TL).

% auxiliary function for check_count
check_count([], []).
check_count([HD|TL], [HD_Num|TL_Num]) :-
    check_count_row(HD, 0, 0, Final_Num),
    (HD_Num #= Final_Num -> 
        check_count(TL, TL_Num);
        fail).

check_count_row([], _, ACC, ACC).
check_count_row([HD_HD|HD_TL], Cur_Max, ACC, Final_Num) :-
    (HD_HD #> Cur_Max -> 
        % if the current one is higher than the previous ones:
        ACC_New is ACC + 1,
        check_count_row(HD_TL, HD_HD, ACC_New, Final_Num);
        % else:
        check_count_row(HD_TL, Cur_Max, ACC, Final_Num)).



% Question 2:
% plain_ntower/3 that acts like ntower/3 but does not use the GNU Prolog finite domain solver.
plain_ntower(0, [], counts([],[],[],[])).
plain_ntower(1, [[1]], counts([1],[1],[1],[1])).
plain_ntower(N, T, C) :-
    N > 1,
    C = counts(Top, Bottom, Left, Right),
    check_row_length(T, N),
    check_col_length(T, N),
    maplist(check_domain_plain(N), T),          % modify this one
    maplist(check_no_dup_plain, T),             % modify this one
    transpose(T, Ttrans),
    maplist(check_domain_plain(N), Ttrans),
    maplist(check_no_dup_plain, Ttrans),
    maplist(fd_labeling_plain(N), T),           % modify this one
    horizontal_flip(T, Tright),
    horizontal_flip(Ttrans, Tbottom),
    check_count(T, Left),
    check_count(Ttrans, Top),
    check_count(Tright, Right),
    check_count(Tbottom, Bottom).


% auxiliary function for check_domain_plain
check_domain_plain(_, []).
check_domain_plain(N, [HDHD|HDTL]) :-
    between(1, N, HDHD),
    check_domain_plain(N, HDTL).

% auxiliary function for check_no_dup_plain
check_no_dup_plain(ROW) :-
    sort(ROW, Sorted),
    length(ROW, Len1),
    length(Sorted, Len2),
    Len1 == Len2.

% auxiliary function for fd_labeling_plain
fd_labeling_plain(N, List) :-
    maplist(between(1, N), List),
    maplist(nonvar, List).


% Speed-up:

test_ntower(T) :-
    statistics(cpu_time, [Start | _]),
    ntower(3, _, counts([2, 2, 1],[2, 1, 3],[3, 1, 2], [1, 2, 2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start + 0.01).

test_plain_ntower(T) :-
    statistics(cpu_time, [Start | _]),
    plain_ntower(3, _, counts([2, 2, 1],[2, 1, 3],[3, 1, 2], [1, 2, 2])),
    statistics(cpu_time, [End | _]),
    T is (End - Start).

speedup(Ratio) :-
    test_ntower(T1),
    test_plain_ntower(T2),
    Ratio is T2/T1.	   


    
% Question 3:
% Write a Prolog predicate ambiguous(N, C, T1, T2) that uses ntower/3 to find a single
% N×N Towers puzzle with edges C and two distinct solutions T1 and T2, and use it to
% find an ambiguous puzzle.

ambiguous(N, C, T1, T2) :-
    ntower(N, T1, C),
    ntower(N, T2, C),
    T1 \= T2.


