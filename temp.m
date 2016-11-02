maxes = zeros(24,1);
indices = zeros(24,1);

for i = 1:24
  
    for j = 1: 24
        if density(i,j) > maxes(i)
            maxes(i)=density(i,j);
            indices(i) = j;
        end
    end
end
