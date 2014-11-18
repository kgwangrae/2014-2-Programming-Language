let val x = (malloc 10, malloc 10) in 
x.1:=read;x.2:=read;
write (!x.1 + !x.2) 
end
