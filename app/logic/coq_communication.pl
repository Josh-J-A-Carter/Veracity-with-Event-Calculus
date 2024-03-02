:- use_module(library(lists)).

% Hold streams to communicate with Coq
:- dynamic coq_process/4.
% Holds bindings used to translate between Prolog terms and Coq terms
:- dynamic proof_bindings/1.
:- dynamic coq_bindings/1.

reset_coq :-
    (coq_process(PID, In, Out, Err)
        -> close(In), close(Out), close(Err), process_kill(PID)
        ; true),
    retractall(coq_process(_, _, _, _)),
    retractall(proof_bindings(_)),
    % Used to store relationships between Prolog terms and Coq terms
    retractall(coq_bindings(_)),
    assert(coq_bindings(_{})).

% When a new Prolog<->Coq binding is created, we need to define it with Coq
assert_definition(Binding, Type) :-
    % Turn this into a valid Coq definition
    (Type = actor 
        -> atomic_list_concat(['Definition ', Binding, ' := Actor "', Binding, '".\r\n'], Definition)
    ; Type = evidence
        -> atomic_list_concat(['Definition ', Binding, ' := AtomicEvid "', Binding, '".\r\n'], Definition)
    ; Type = claim
        -> atomic_list_concat(['Definition ', Binding, ' := AtomicClaim "', Binding, '".\r\n'], Definition)),
    % Send it to Coq & clear the response in the Out stream
    get_stream_handles(In, Out, _),
    write(In, Definition), flush_output(In),
    fill_buffer(Out), read_pending_codes(Out, _, _).

% verify(+Judgement, -Success, -Error_Location)
verify(Judgement, Success, Error_Location) :-
    get_stream_handles(In, Out, Err),
    get_tactics(Judgement, Tactics),
    
    % Recursively verify each tactic, starting from line 0
    verify_loop(Tactics, In, Out, Err, 0, Error),
    % Error of -1 means success
    (Error = -1
        -> Success = true, Error_Location = -1
        % We have to shift the error location back by three because we always add three tactics to the start of proofs
        ; Success = false, Error_Location is Error - 3), !.

verify_loop([Next | Rest], In, Out, Err, Curr_Pos, Error_Location) :-
    
    % Messages marked as 'ignore' won't produce output from Coq, so we can't read the buffer
    % due to Prolog lacking a non-blocking reading predicate for an empty stream
    (Next = ignore(Input) ; Next = Input),
    
    % Send the next tactic to Coq
    atomic_list_concat([Input, '\r\n'], Message),
    write(In, Message), flush_output(In),

    % Check if there are any errors
    % Note that Coq is using stderr for both actual errors, and the terminal prompt
    % so first we need to clear stderr, then check for instances of 'Error'
    fill_buffer(Err), read_pending_codes(Err, _, _),
    fill_buffer(Err), read_pending_codes(Err, Error_Codes, []), string_codes(Error_Message, Error_Codes),
    !, (
        % An error has occurred, so abort the proof and return the error location
        sub_string(Error_Message, _, _, _, "Error")
            -> (write(In, 'Abort.\r\n'), flush_output(In), Error_Location is Curr_Pos)

        % Or, no error has occurred, but the message we've just inputted won't give us any response
        % meaning that we shouldn't read the buffer, or the thread will block
        ; Next = ignore(_)
            -> verify_loop(Rest, In, Out, Err, Curr_Pos + 1, Error_Location)

        % Otherwise, no error and we can safely read the buffer before recursing
        ; fill_buffer(Out), read_pending_codes(Out, _, []),
        verify_loop(Rest, In, Out, Err, Curr_Pos + 1, Error_Location)
    ).
% If we complete the proof without errors, then Error_Location is -1
verify_loop([], _, _, _, _, -1).


% get_stream_handles(-In, -Out, -Err)
get_stream_handles(In, Out, Err) :-
    % Get handles if they exist
    coq_process(_, In, Out, Err) -> true

    % Otherwise, start a new process
    ; process_create(path(coqtop), ['-load-vernac-source', 'logic/VeracityLogicV3.v'], 
                    [process(PID), stdin(pipe(In)), stdout(pipe(Out)), stderr(pipe(Err))]),
    % Ignore anything left in the buffers (e.g. welcome message)
    fill_buffer(Err), read_pending_codes(Err, _, _),
    fill_buffer(Out), read_pending_codes(Out, _, _),
    % Assert the stream handles for later use
    assert(coq_process(PID, In, Out, Err)).

get_tactics(judgement(Actor, Proof, Claim)=_, Tactics) :-
    % These are all using Prolog terms, which aren't always compatible with Coq, so we need to translate them
    prolog_to_coq_claim(Claim, Coq_Claim),
    get_coq_binding(Actor, actor, Coq_Actor),
    next_proof_binding(Coq_Proof),
    prolog_to_coq_tactics(Proof, [], Main_Tactics),


    % Need to add a few extra tactics for defining the proof, initial goal, and printing the proof tree
    Prelude = [Definition, ignore('Proof.'), Existential],
    Postlude = [ignore('Defined.'), Evaluation],

    atomic_list_concat(['Definition ', Coq_Proof, ' : proofTreeOfClaim ', Coq_Claim, '.'], Definition),
    atomic_list_concat(['eexists _ ', Coq_Actor, '.'], Existential),
    atomic_list_concat(['Eval compute in show ', Coq_Proof, '.'], Evaluation),

    append([Prelude, Main_Tactics, Postlude], Tactics).

% Turn the prolog representation of tactics into actual Coq lines
prolog_to_coq_tactics([Prolog | Rest], Tactic_Acc, Tactics) :-
    (
        Prolog = impl_elim(Actor, _, _, _, _, ((_ ==> Cons_Claim) - Ant_Claim))
            ->  prolog_to_coq_claim(Cons_Claim, Con),
                prolog_to_coq_claim(Ant_Claim, Ant),
                get_coq_binding(Actor, actor, A),
                atomic_list_concat(['eapply (impl_elim ', A, ' _ _ (', Ant, ') (', Con,')).'], Tactic)
        ; Prolog = trust(Actor1, Actor2, _, _, _)
            ->  get_coq_binding(Actor1, actor, A1),
                get_coq_binding(Actor2, actor, A2),
                atomic_list_concat(['eapply (trust ', A1, ' ', A2, ' _ _ (Trust "T")).'], Tactic)
        ; Prolog = assume(Evidence, Actor, _)
            ->  get_coq_binding(Evidence, evidence, E),
                get_coq_binding(Actor, actor, A),
                atomic_list_concat(['eapply (assume (', E, ') ', A, ').'], Tactic)
        ; Prolog = and_intro(_, _, _, _, _, _) ->  Tactic = 'eapply and_intro.'
        ; Prolog = or_intro1(_, _, _, _) ->  Tactic = 'eapply or_intro1.'
        ; Prolog = or_intro2(_, _, _, _) ->  Tactic = 'eapply or_intro2.'
        ; Prolog = leaf -> Tactic = 'eapply leaf.'
    ),
    append(Tactic_Acc, [Tactic], Tactic_Next),
    prolog_to_coq_tactics(Rest, Tactic_Next, Tactics).
prolog_to_coq_tactics([], Tactics, Tactics).

prolog_to_coq_claim(Prolog_Claim, Coq_Claim) :-
    (( % Implication
        Prolog_Claim = (Ant ==> Con),
        % Parse left and right children
        prolog_to_coq_claim(Ant, Coq_Ant),
        prolog_to_coq_claim(Con, Coq_Con),
        % Return value
        atomic_list_concat(['(Implies (', Coq_Ant, ') (', Coq_Con, '))'], Coq_Claim)
    )
    ; ( % Conjunction
        Prolog_Claim = (A, B),
        prolog_to_coq_claim(A, Coq_A),
        prolog_to_coq_claim(B, Coq_B),
        atomic_list_concat(['(', Coq_A, ' /\\\' ', Coq_B, ')'], Coq_Claim)
    )
    ; ( % Disjunction
        Prolog_Claim = (A ; B),
        prolog_to_coq_claim(A, Coq_A),
        prolog_to_coq_claim(B, Coq_B),
        atomic_list_concat(['(', Coq_A, ' \\/\' ', Coq_B, ')'], Coq_Claim)
    )
    ; ( % Atomic claim - make sure this is defined for Coq, otherwise it will complain
        get_coq_binding(Prolog_Claim, claim, Coq_Claim)
    )), !.

bind_variables([], _) :- !.
bind_variables([Unbound | Rest], Count) :-
    atomic_list_concat([var, Count], Unbound),
    Next is Count + 1, !,
    bind_variables(Rest, Next).

% Count the number of proofs so that we don't accidentally re-define existing ones
next_proof_binding(Binding) :-
    proof_bindings(N)
        -> Succ is N + 1, atomic_list_concat(['P', Succ], Binding),
        retractall(proof_bindings(_)), assert(proof_bindings(Succ))

        ; Succ is 1, atomic_list_concat(['P', Succ], Binding),
        assert(proof_bindings(Succ)).

% Prolog terms aren't all valid Coq, so we need to translate between them
% Note that the Coq bindings are arbitrary (as long as they're valid and unique, of course)
get_coq_binding(Term, Type, Binding) :-
    % Don't want to bind the original term's variables, but we do need to bind them for the sake of the dictionary
    % Otherwise, we could have claim(X, Y) ==> 'claim(_1, _2)', and another time it could become 'claim(_3, _4)' which don't unify!
    copy_term(Term, Term_Copy),
    term_variables(Term_Copy, Unbound),
    bind_variables(Unbound, 1),
    % Turn this into an atom (dictionaries don't like complex terms...)
    term_to_atom(Term_Copy, Key),
    % Get the existing bindings
    coq_bindings(In),
    % Get an existing binding that matches, or create a new one
    (Type = actor -> (
        Binding = In.get(actor_bindings/Key), In = Out
        ;
        Num = In.get(actor_bindings_num, 1),
        New_Num is Num + 1,
        atomic_list_concat(['a', Num], Binding),
        Inter = In.put(actor_bindings/Key, Binding),
        Out = Inter.put(actor_bindings_num, New_Num),
        % Send the definition to Coq
        assert_definition(Binding, Type), !
    )
    ; Type = evidence -> (
        Binding = In.get(evidence_bindings/Key), In = Out
        ;
        Num = In.get(evidence_bindings_num, 1),
        New_Num is Num + 1,
        atomic_list_concat(['e', Num], Binding),
        Inter = In.put(evidence_bindings/Key, Binding),
        Out = Inter.put(evidence_bindings_num, New_Num),
        % Send the definition to Coq
        assert_definition(Binding, Type), !
    )
    ; Type = claim -> (
        Binding = In.get(claim_bindings/Key), In = Out
        ;
        Num = In.get(claim_bindings_num, 1),
        New_Num is Num + 1,
        atomic_list_concat(['C', Num], Binding),
        Inter = In.put(claim_bindings/Key, Binding),
        Out = Inter.put(claim_bindings_num, New_Num),
        % Send the definition to Coq
        assert_definition(Binding, Type), !
    )),
    % Update the stored bindings
    retractall(coq_bindings(_)), assert(coq_bindings(Out)), !.

