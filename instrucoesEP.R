dados <- rnorm()
encontrar dados
fazer a funcao de distribuicao acumulada dos nossos dados
pra cada distribuicao, vamos criar um vetor aleatorio daquela
distribuicao com os parametros dos nossos dados
ex.: parametros da normal: media e desvio padrao
criemos uma curva normal aleatoria a partir dos nossos dados, com a meida
e o desvio padrao dos nossos dados
fazemos a funcao de distribuicao acumulada do vetor que criamos, isto 
para cda vetor de distribuicao diferente criado

Vamos ter 2 vetores: o com os nossos dados e o aleatorio, que tem o mesmo tamanho
vamos percorrer esses 2 vetores
e vamos aplicar uma formula
se a diferenca dos dois valores for maior que o valor estimpulado, que geralmentye eh 0.05,
nao eh adqueado

esse processo de desde a parte de criar o valor aleatorio da dsitribuicao,
vamos repetir esse processo 100, 1000 vezes, pq eh a aplicacao do metodo de monte carlo,
que eh uma simulacao, pq simuladmos 1000, 100, 500 , vezes
temos que comparar os resultados de cada distribuicxao que vamos testar, e ver
qual foi mais proxima
