%-----------------------------------------------------------------------%
% Plasma types representation
% vim: ts=4 sw=4 et
%
% Copyright (C) 2017, 2020-2021 Plasma Team
% Distributed under the terms of the MIT License see ../LICENSE.code
%
%-----------------------------------------------------------------------%
:- module core.resource.
%-----------------------------------------------------------------------%

:- interface.

:- import_module context.
:- import_module common_types.
:- import_module q_name.

%-----------------------------------------------------------------------%

:- type resource
    --->    r_io
    ;       r_other(
                ro_name     :: q_name,
                ro_from     :: resource_id,
                ro_sharing  :: sharing,
                ro_context  :: context
            )
            % A resource imported from another module, this may only exist
            % during interface generation.
    ;       r_abstract(q_name).

:- func resource_to_string(resource) = string.

:- pred resource_is_decendant(core::in, resource::in, resource_id::in)
    is semidet.

%-----------------------------------------------------------------------%
%-----------------------------------------------------------------------%

:- implementation.

:- import_module builtins.

%-----------------------------------------------------------------------%

resource_to_string(r_io) =
    q_name_to_string(q_name_append(builtin_module_name, nq_name_det("IO"))).
resource_to_string(r_other(Symbol, _, _, _)) = q_name_to_string(Symbol).
resource_to_string(r_abstract(Symbol)) = q_name_to_string(Symbol).

resource_is_decendant(_, r_io, _) :- false.
resource_is_decendant(Core, r_other(_, Parent, _, _), Ancestor) :-
    (
        Parent = Ancestor
    ;
        resource_is_decendant(Core, core_get_resource(Core, Parent), Ancestor)
    ).

%-----------------------------------------------------------------------%
