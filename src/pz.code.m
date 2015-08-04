%-----------------------------------------------------------------------%
% vim: ts=4 sw=4 et
%-----------------------------------------------------------------------%
:- module pz.code.
%
% PZ representation of code.
%
% Copyright (C) 2015 Paul Bone
% Distributed under the terms of the GPLv2 see ../LICENSE.tools
%
%-----------------------------------------------------------------------%
:- interface.

:- import_module list.

:- type pz_proc
    --->    pz_proc_defn(
                pzdf_signature      :: pz_signature,
                pzdf_instrs         :: list(pz_instr)
            )
            % A declration for a procedure in another module.
    ;       pz_proc_decl(
                pzdc_signature      :: pz_signature
            ).

    % A procedure's signature describes how it behaves with respect to the
    % parameter stack.
    %
    %   ( before - after )
    %
    % before is the list of items (left = lower) on the stack before the
    % call, after is the list of items (left = lower) on the stack after the
    % call.  Of course other things may be on the stack, but this call
    % promises no to affect them.
    %
    % The bytecode interpreter/code generator isn't required to check this,
    % but it may use this information to generate code - so it must be
    % correct.
    %
    % XXX: varargs
    %
:- type pz_signature
    --->    pz_signature(
                pzs_before      :: list(pz_data_width),
                pzs_after       :: list(pz_data_width)
            ).

:- type pz_instr
    --->    pzi_load_immediate_8(int)
    ;       pzi_load_immediate_16(int)
    ;       pzi_load_immediate_32(int)
    ;       pzi_load_immediate_64(int)
    ;       pzi_load_immediate_ptr(int)
    ;       pzi_load_data(pzd_id)
    ;       pzi_call(pzp_id).

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%
