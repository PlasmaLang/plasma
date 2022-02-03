%-----------------------------------------------------------------------%
% Timing utility code
% vim: ts=4 sw=4 et
%
% Copyright (C) 2021-2022 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module util.my_time.

:- interface.

:- import_module io.
:- import_module string.

%-----------------------------------------------------------------------%

:- type time2(T)
    --->    time2(
                t_real_time        :: T,
                t_cpu_time         :: T
            ).

:- type timestamp.

:- type duration.

%-----------------------------------------------------------------------%

:- pred now(time2(timestamp)::out, io::di, io::uo) is det.

:- func diff_time(time2(timestamp), time2(timestamp)) = time2(duration).

:- func str_time(duration) = string.

:- func format_duration(time2(duration)) = string.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
:- implementation.

:- import_module float.
:- import_module int64.
:- import_module list.

:- type timestamp == timespec.

:- type duration == timespec.

%-----------------------------------------------------------------------%

:- pragma foreign_decl("C",
    "#include <time.h>").

:- type clock_id
    --->    clock_real
    ;       clock_cpu.

:- pragma foreign_enum("C", clock_id/0,
    [
        clock_real  - "CLOCK_MONOTONIC",
        % Linux specific
        clock_cpu   - "CLOCK_PROCESS_CPUTIME_ID"
    ]).

:- type timespec.

:- pragma foreign_type("C", timespec, "struct timespec").

:- pred timespec(timespec, int64, int64).
:- mode timespec(in, out, out) is det.
:- mode timespec(out, in, in) is det.

:- pragma foreign_proc("C",
    timespec(TS::in, Secs::out, NSecs::out),
    [promise_pure, thread_safe, will_not_throw_exception,
     will_not_call_mercury],
    "
        Secs  = TS.tv_sec;
        NSecs = TS.tv_nsec;
    ").

:- pragma foreign_proc("C",
    timespec(TS::out, Secs::in, NSecs::in),
    [promise_pure, thread_safe, will_not_throw_exception,
     will_not_call_mercury],
    "
        TS.tv_sec  = Secs;
        TS.tv_nsec = NSecs;
    ").

%-----------------------------------------------------------------------%

now(time2(Real, CPU), !IO) :-
    now(clock_real, Real, !IO),
    now(clock_cpu, CPU, !IO).

:- pred now(clock_id::in, timestamp::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    now(Clock::in, Time::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_throw_exception,
     will_not_call_mercury],
    "
        int ret = clock_gettime(Clock, &Time);
        if (ret != 0) {
            perror(""Warning, clock_gettime"");
            Time.tv_sec = 0;
            Time.tv_nsec = 0;
        }
    ").

%-----------------------------------------------------------------------%

diff_time(time2(RealAfter, CPUAfter), time2(RealBefore, CPUBefore)) =
    time2(RealAfter - RealBefore, CPUAfter - CPUBefore).

:- func timespec - timespec = timespec.

T1 - T2 = T :-
    timespec(T1, T1Sec0, T1NSec0),
    timespec(T2, T2Sec, T2NSec),

    ( if T1NSec0 < T2NSec then
        % Need to carry.
        T1Sec = T1Sec0 - 1i64,
        T1NSec = T1NSec0 + 1_000_000_000i64
    else
        T1Sec = T1Sec0,
        T1NSec = T1NSec0
    ),
    timespec(T, T1Sec - T2Sec, T1NSec - T2NSec).

%-----------------------------------------------------------------------%

str_time(Duration) = format("%.2f%s", [f(Float), s(Unit)]) :-
    timespec(Duration, Secs, NSecs),
    ( if Secs > 0i64 then
        Float = float.cast_from_int64(Secs) +
            (float.cast_from_int64(NSecs) / 1_000_000_000.0),
        Unit = "s"
    else if NSecs > 1_000_000i64 then
        Float = float.cast_from_int64(NSecs) / 1_000_000.0,
        Unit = "ms"
    else if NSecs > 1_000i64 then
        Float = float.cast_from_int64(NSecs) / 1_000.0,
        Unit = "us"
    else
        Float = float.cast_from_int64(NSecs),
        Unit = "ns"
    ).

format_duration(Time) =
    format("real: %s, cpu: %s",
        [s(str_time(Time ^ t_real_time)),
         s(str_time(Time ^ t_cpu_time))]).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
