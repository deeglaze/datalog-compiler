#lang racket/base
(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/cvector
         ffi/unsafe/define)
(provide (all-defined-out))

(define-cstruct _statsblk
  ([grpsize1 _double]
   [grpsize2 _int64]
   [numorbits _int64]
   [numgenerators _int64]
   [errstatus _int64]
   [numnodes _ulong]
   [numbadleaves _ulong]
   [maxlevel _int64]
   [tctocal _ulong]
   [canupdates _ulong]
   [invapplics _ulong]
   [invsuccesses _ulong]
   [invarsuclevel _int64]))

(define setword _uint64) ;; wordsize==64
(define _size_t _uint64)
(define _graph setword)
(define _set setword)
(define _graph-ptr (_cvector i)) ;; setword
(define _set-iptr (_ptr i _set))
(define _set-ioptr (_ptr io _set))
(define _set-optr (_ptr o _set))
(define _set-ptr _pointer)

(define _level _int64)
(define _active _set-ptr) ;; workspace

(define _m _int64) ;; setwords needed given _n
(define _n _int64) ;; number of graph vertices
(define _tc_level _int64) ;; input option
#| [verbatim from nautil.c:]
   The intention of hint is that, if hint >= 0 and there
*  is a suitable non-trivial cell starting at position hint in lab,
*  that cell is chosen.
|#
(define _hint _int64)
 ;; Single ints that are read and written
(define _numcells _int64)
(define _*numcells (_ptr io _numcells))
(define _qinvar (_ptr io _int64))
(define _code (_ptr io _int64))

(define _workperm _set-ptr) ;; global workspace

(define _refproc+
  (_fun _graph-ptr
        (_cvector io) (_cvector io) ;; lab/ptn
        _level _*numcells
        (_cvector io) ;; _invar
        _active _code _m _n -> _void))
(define _refproc-
  (_fun _pointer
        _pointer _pointer
        _level _pointer
        _pointer ;; _invar
        _active _intptr _m _n -> _void))

(define _targetcell+
  (_fun _graph-ptr
        (_cvector io) (_cvector io) ;; lab/ptn
        _level _tc_level _bool _hint _m _n -> _int64))
(define _targetcell-
  (_fun _pointer ;; graph
        _pointer _pointer ;; lab/ptn
        _level _tc_level _bool _hint _m _n -> _int64))

(define _updatecan+
  (_fun _graph-ptr _graph-ptr
        _intptr _int64 _int64 _int64 -> _void))
(define _updatecan-
  (_fun _pointer _pointer ;; _graph-ptr _graph-ptr
        _intptr _int64 _int64 _int64 -> _void))

(define _testcanlab+
  (_fun _graph-ptr _graph-ptr
        _intptr _intptr _int64 _int64 -> _int64))
(define _testcanlab-
  (_fun _pointer _pointer ;; _graph-ptr _graph-ptr
        _intptr _intptr _int64 _int64 -> _int64))

(define _isautom+
  (_fun _graph-ptr
        _workperm _bool _m _n -> _bool))
(define _isautom-
  (_fun _pointer ;; _graph-ptr
        _workperm _bool _m _n -> _bool))

;; first argument is ptn
(define _cheapautom+
  (_fun (_cvector io) _level _bool _n -> _bool))
(define _cheapautom-
  (_fun _pointer _level _bool _n -> _bool))

(define _check
  (_fun _int64 _int64 _int64 _int64 -> _void))

(define-cstruct _dispatchvec
  ([isautom _isautom-]
   [testcanlab _testcanlab-]
   [updatecan _updatecan-]
   [refine _refproc-]
   [refine1 _refproc-]
   [cheapautom _cheapautom-]
   [targetcell _targetcell-]
   [freedyn (_fun -> _void)]
   [check _check]
   [init (_fun _pointer ;; _graph-ptr ;; g_arg
               _pointer ;; (_ptr o _graph-ptr) ;; &g
               _pointer ;; _graph-ptr ;; canong_arg
               _pointer ;; (_ptr o _graph-ptr) ;; &canong
               _pointer _pointer ;; lab/ptn
               _set-ptr ;; active
               _pointer ;; _optionstruct-pointer/null
               _intptr ;; initstatus
               _int64 _int64 -> _void)]
   [cleanup (_fun _pointer ;; _graph-ptr
                  _pointer ;; (_ptr o _graph-ptr)
                  _pointer ;; _graph-ptr
                  _pointer ;; (_ptr o _graph-ptr)
                  _pointer _pointer ;; lab/ptn
                  _pointer ;; _optionstruct-pointer
                  _pointer ;; _statsblk-pointer
                  _m _n -> _void)]))

;; Callbacks have cvector container stripped away.
(define _invarproc-
  (_fun _pointer ;; graph
        _pointer _pointer ;; lab/ptn
        _level _int64 _int64 _intptr _int64 _bool _int64 _int64 -> _void))

(define _automproc-
  (_fun _int64 ;; numgenerators
        _pointer ;; read in orbjoin
        _intptr ;; global int *orbits in nauty.c
        ;; numorbits, stabvertex, n
        _int64 _int64 _int64 -> _void))

(define _levelproc-
  (_fun _pointer _pointer ;; lab/ptn
        _level
        _intptr ;; global int *orbits in nauty.c
        _statsblk-pointer
        ;; tv1, index, tcellsize, numcells, childcount, n
        _int64 _int64 _int64 _int64 _int64 _int64 -> _void))

(define _nodeproc-
  (_fun _pointer ;; _graph-ptr
        _pointer _pointer ;; lab/ptn
        _level
        ;; numcells, tc, code
        _int64 _int64 _int64
        _m _n -> _void))

(define-cstruct _optionstruct
  ([getcanon _int64]
   [digraph _bool]
   [writeautoms _bool]
   [writemarkers _bool]
   [defaultptn _bool]
   [cartesian _bool]
   [linelength _int64]
   [outfile _pointer]
   [userrefproc _refproc-]
   [userautomproc _automproc-]
   [userlevelproc _levelproc-]
   [usernodeproc _nodeproc-]
   [invarproc _invarproc-]
   [tc_level _tc_level]
   [mininvarlevel _int64]
   [maxinvarlevel _int64]
   [invararg _int64]
   [dispatch _dispatchvec-pointer]
   [schreier _bool]
   [extra_options _pointer]))

(define-cstruct _sparsegraph
  ([nde _size_t]
   [v (_ptr io _size_t)]
   [nv _int64]
   [d (_ptr io _int64)]
   [e (_ptr io _int64)]
   [w _pointer]
   [vlen _size_t]
   [dlen _size_t]
   [elen _size_t]
   [wlen _size_t]))

(define libnauty (ffi-lib "nauty"))
(define-ffi-definer define-nauty libnauty)

(define-nauty alloc_error (_fun _string -> _void))
(define-nauty breakout (_fun _intptr _intptr ;; lab/ptn
                             _int64 _int64 _int64
                             ;; *active immediately emptied.
                             _set-optr _int64 -> _void))
(define-nauty cheapautom _cheapautom+)
(define-nauty doref
  (_fun _graph-ptr
        (_cvector io) (_cvector io) ;; lab/ptn
        _level
        ;; numcells, qinvar
        _*numcells (_ptr o _int64)
        _workperm
        ;; active
        _set-ptr
        ;; code
        _code
        _refproc-
        _invarproc-
        _int64 _int64 _int64 _bool _m _n -> _void))
(define-nauty extra_autom (_fun (_cvector i) ;;  _int64
                                _int64 -> _void))
(define-nauty extra_level (_fun _level
                                (_cvector io) (_cvector io) ;; lab/ptn
                                _numcells _int64 _int64 _int64 _int64 _int64
                                -> _void))
(define-nauty isautom _isautom+)
(define-nauty dispatch_graph _dispatchvec)
;; Writes to the character array
(define-nauty itos (_fun _int64 _pointer -> _int64))
;; first pointer is read-only array
(define-nauty fmperm (_fun _intptr _set-optr _set-optr _int64 _int64 -> _void))
;; First two int pointers are read-only arrays.
(define-nauty fmptn (_fun _intptr _intptr _int64 _set-optr _set-optr _int64 _int64 -> _void))
;; longprune writes to first set, reading from second and third, only comparing pointer for fourth.
(define-nauty longprune (_fun _set-ptr _set-iptr _cvector _set-iptr _m -> _void))
(define-nauty nauty (_fun _graph-ptr
                          (_cvector io) (_cvector io) ;; lab/ptn
                          _set-ptr      ;; active_arg only read
                          (_cvector io) ;; orbits_arg only written
                          _optionstruct-pointer
                          _statsblk-pointer
                          _pointer _int64 ;; workspace memory and size
                          _int64 _int64   ;; m and num-vertices
                          (_cvector io)   ;; output canonical graph
                          -> _void))
(define-nauty maketargetcell
  (_fun _graph-ptr
        (_cvector io) (_cvector io) ;; lab/ptn
        _int64
        (tcell : _set-optr)
        (tcellsize : (_ptr o _int64))
        (cellpos : (_ptr o _int64))
        _int64 _bool _int64 _targetcell- _int64 _int64 -> _void ->
        (values tcell tcellsize cellpos)))
(define-nauty nextelement (_fun _set-iptr _int64 _int64 -> _int64)) ;; pure
#|
*  orbits represents a partition of {0,1,...,n-1}, by orbits[i] = the
*  smallest element in the same cell as i.  map[] is any array with values
*  in {0,1,...,n-1}.  orbjoin(orbits,map,n) joins the cells of orbits[]
*  together to the minimum extent such that for each i, i and map[i] are in
*  the same cell.  The function value returned is the new number of cells.
|#
(define-nauty orbjoin (_fun (_cvector io) (_cvector i) ;; both _int64
                            _int64 -> _int64))
#|
*  permset(set1,set2,m,perm)  defines set2 to be the set
*  {perm[i] | i in set1}.
|#
(define-nauty permset (_fun _set-iptr _set-optr _int64 (_ptr i _int64) -> _void))
;; Writes a string to the given function pointer
(define-nauty putstring (_fun _pointer _string -> _void))
(define-nauty refine _refproc+)
(define-nauty refine1 _refproc+)
;; shortprune(set1,set2,m) ANDs the contents of set set2 into set set1.
(define-nauty shortprune (_fun _set-ioptr _set-iptr _int64 -> _void))
(define-nauty targetcell _targetcell+)
(define-nauty testcanlab _testcanlab+)
(define-nauty updatecan _updatecan+)
(define-nauty writeperm (_fun _pointer _intptr _bool _int64 _int64 -> _void))
(define-nauty nauty_freedyn (_fun -> _void))
(define-nauty nauty_check _check)
(define-nauty naugraph_check _check)
(define-nauty nautil_check _check)
(define-nauty nautil_freedyn (_fun -> _void))
(define-nauty naugraph_freedyn (_fun -> _void))

(define-nauty adjacencies (_fun _pointer ;; _graph-ptr
                                _pointer _pointer ;; lab/ptn
                                _int64 _int64 _int64
                                _pointer ;; invar
                                _int64
                                _bool _int64 _int64 ->
                                _void))
;; Not in Ubuntu distribution of libnauty
(define-nauty densenauty (_fun _graph-ptr
                               (_cvector io) (_cvector io) ;; lab/ptn
                               _cvector
                               _optionstruct-pointer
                               _statsblk-pointer
                               _int _int
                               _graph-ptr -> _void))
(define-nauty liason (_fun _graph-ptr
                           _cvector _cvector ;; lab/ptn
                           _cvector
                           _int _int
                           _graph-ptr -> _void))
(define-nauty dense_compare (_fun _graph-ptr _graph-ptr _int _int -> _bool))
(define-nauty writegroupsize (_fun _pointer _double _int -> _void))

(define bit
  (let ([a (make-cvector setword 64)])
    (for ([i (in-range 64)])
      (cvector-set! a i (arithmetic-shift 1 (- 63 i))))
    a))

(define NULL #f)

(define (default-options-digraph)
  (make-optionstruct 0
                     #t ;; is a digraph
                     #f #f ;; writeautoms/writemarkers
                     #t ;; defaultptn
                     #f ;; cartesion
                     78 ;;CONSOLWIDTH linelength
                     NULL ;; outfile
                     (function-ptr NULL _refproc-)
                     (function-ptr NULL _automproc-)
                     (function-ptr NULL _levelproc-)
                     (function-ptr NULL _nodeproc-)
                     ;; invarproc
                     adjacencies
                     100 ;; tc_level
                     0 999 ;; min/max invarlevel
                     0 dispatch_graph #f NULL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A little bit of ported C code for constructing the graphs

(define WORDSIZE 64)
(define NAUTYVERSIONID 25490)
(define (SETWORDSNEEDED n) (add1 (arithmetic-shift (sub1 n) -4))) ;; ((((n)-1)>>4)+1)
#|because wordsize == 64 |#
(define (SETWD pos) (arithmetic-shift pos -6))
(define (SETBT pos) (bitwise-and pos #x3f))
(define (TIMESWORDSIZE w) (arithmetic-shift w 6))

(define (ADDELEMENT0 setadd pos)
  ;; ((setadd)[SETWD(pos)] |= BITT[SETBT(pos)])
  (define idx (SETWD pos))
  (ptr-set! setadd setword idx
            (bitwise-ior (ptr-ref setadd setword idx)
                         (cvector-ref bit (SETBT pos)))))
(define (ADDONEARC g v w m)
  (ADDELEMENT0 (GRAPHROW0 g v m) w))
(define (GRAPHROW0 g v m)
  ;;((set*)(g) + (m)*(size_t)(v))
  (ptr-add (cvector-ptr g) (* m v) setword))

(define (EMPTYSET0 setadd m)
  (memset (cvector-ptr setadd) 0 m setword))
