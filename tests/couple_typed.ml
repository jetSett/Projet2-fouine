let (f : int ref), (g : int ref ref) = ref 0, ref (ref 42) in !f, !!g;;
