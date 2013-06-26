rce - riak_core example
=======================

### usage 

```bash
☄ git clone git@github.com:fabiankrol/rce.git
☄ cd rce/
☄ make all
```

### game
```
☄ make run
Erlang R15B01 (erts-5.9.1) [source] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
(rce@127.0.0.1)1> B = rce_game:new().
#Ref<0.0.0.724>
(rce@127.0.0.1)2> rce_game:move(B, {1,1}, {1,1}, x).
ok
(rce@127.0.0.1)3> rce_game:move(B, {1,1}, {1,1}, o).
{error,forbidden}
(rce@127.0.0.1)4> rce_game:move(B, {1,1}, {1,2}, o).
ok
...
(rce@127.0.0.1)N> rce_game:move(B, {2,3}, {3,3}, x).
{ok,{win,x}}
```

### loosely based on

http://mathwithbaddrawings.com/2013/06/16/ultimate-tic-tac-toe/
