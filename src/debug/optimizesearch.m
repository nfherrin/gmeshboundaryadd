clear;
i=1;
numelements=10;
j=i;
jup=i;
jdown=i;
for m=1:numelements
    fprintf('%i\n',j);
    if mod(m,2) == 0
        jup=jup+1;
        if jup >= numelements+1
            jdown=jdown-1;
            j=jdown;
        else
            j=jup;
        end
    else
        jdown=jdown-1;
        if jdown <= 0
            jup=jup+1;
            j=jup;
        else
            j=jdown;
        end
    end
end