


# posprocsmap

Este repositorio contem o ferramental para estudo de motodologias de pos processamento de diferentes 
previsoes de vazao diaria. Aqui se encontram facilitadores da leitura dos dados
organizados em um bucket s3, implementacoes de modelos para pos processamento e execucoes em janela 
movel para avaliacao de desempenho passado.


## Leitura dos dados

Ha diversas funcoes de leitura para aquisicao dos dados no bucket:

* le_parametros
* le_psat
* le_previstos
* le_vazoes
* le_assimilacao

Cada uma delas acessa um tipo especifico de dado, indicado pelo nome. Todas elas retornam 
`data.table`s com o dado requisitado, exceto por `le_parametros` que retorna uma lista. A seguir
encontram-se exemplos de uso. Deve ser notado que **todas as sub-bacias sao referenciadas por seu 
codigo em caixa alta**.

Para leitura de parametros calibrados:


```r
# Para a subbacia AVERMELHA
params <- le_parametros("AVERMELHA")
str(params)
#> List of 1
#>  $ subbacia_1:List of 22
#>   ..$ id_subbacia: int 1
#>   ..$ codigo     : chr "AVERMELHA"
#>   ..$ area       : int 20922
#>   ..$ kt         : num [1:5] 0.25 0.3 0.25 0.2 0
#>   ..$ str        : int 137
#>   ..$ k2t        : int 5
#>   ..$ crec       : int 22
#>   ..$ ai         : int 1
#>   ..$ capc       : int 30
#>   ..$ kkt        : int 143
#>   ..$ k2t2       : int 10
#>   ..$ H1         : int 200
#>   ..$ H          : int 200
#>   ..$ k3t        : int 10
#>   ..$ k1t        : int 10
#>   ..$ ecof       : num 0.83
#>   ..$ pcof       : num 0.9
#>   ..$ ecof2      : int 0
#>   ..$ sup_ebin   : num 1.2
#>   ..$ inf_ebin   : num 0.8
#>   ..$ sup_prec   : int 2
#>   ..$ inf_prec   : num 0.5
```

Leitura de dados tabelados (por exemplo o previsto):


```r
# Para a subbacia AVERMELHA nos horizontes D+1 a D+5
prevs <- le_previstos("AVERMELHA", horizonte = seq_len(3))
print(prevs)
#>        id_subbacia id_modelo data_previsao dia_previsao precipitacao  vazao
#>     1:           1         1    2011-01-01            1         7.18 320.37
#>     2:           1         1    2011-01-01            2        35.27 324.25
#>     3:           1         1    2011-01-01            3         3.48 342.27
#>     4:           1         1    2011-01-02            1        35.27 315.34
#>     5:           1         1    2011-01-02            2         3.48 332.43
#>    ---                                                                     
#> 11918:           1         1    2021-11-15            2         0.84 137.89
#> 11919:           1         1    2021-11-15            3         4.76 129.58
#> 11920:           1         1    2021-11-16            1         0.84 141.80
#> 11921:           1         1    2021-11-16            2         4.76 132.99
#> 11922:           1         1    2021-11-16            3        23.48 131.72
```
