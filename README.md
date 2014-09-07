# ID3

Builds a decision tree from a fixed set of examples using the
[ID3 algorithm](http://en.wikipedia.org/wiki/ID3_algorithm) by
[Ross Quinlan](http://en.wikipedia.org/wiki/Ross_Quinlan). The
resulting tree may then be used to classify future samples.


## Building

Uses [erlang.mk](https://github.com/ninenines/erlang.mk). To build run `make`.

## Usage

Examples are key value pairs that are classified.


```erlang
Examples = [
{[{hair, blonde}, {height, average},
	{weight, light}, {lotion, no}], sunburned},

{[{hair, blonde}, {height, tall},
	{weight, average}, {lotion, yes}], none},

{[{hair, brown}, {height, short},
	{weight, average}, {lotion, yes}], none},

{[{hair, blonde}, {height, short},
	{weight, average}, {lotion, no}], sunburned},

{[{hair, red}, {height, average},
	{weight, heavy}, {lotion, no}], sunburned},

{[{hair, brown}, {height, tall},
	{weight, heavy}, {lotion, no}], none},

{[{hair, brown}, {height, average},
	{weight, heavy}, {lotion, no}], none},

{[{hair, blonde}, {height, short},
	{weight, light}, {lotion, yes}], none}].

```

The decision tree is built from a set of classified examples:

```erlang
Tree = quinlan_id3:tree([quinlan_id3:classify(Example, Classification) || {Example, Classification} <- Examples]).
```

Once built the tree can be walked:

```erlang
sunburned = quinlan_id3:walk([{hair, red}], Tree).

none = quinlan_id3:walk([{hair, brown}], Tree).

sunburned = quinlan_id3:walk([{hair, blonde}, {lotion, no}], Tree).

none = quinlan_id3:walk([{hair, blonde}, {lotion, yes}], Tree).
```


