"Add this to the SequenceableCollection: "
SequenceableCollection>>bogoSort
    "A simple bogosort."
    [ self isSorted ] whileFalse: [ 
        self shuffle.
    ]

"Then you can run this anywhere: "
#(4 3 2 1 6 5) bogoSort "#(1 2 3 4 5 6)"
