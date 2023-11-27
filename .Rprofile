source("renv/activate.R")

options(
    max.print = 500,
    vsc.dev.args = list(width = 1200, height = 800)
)

# VARIAVEIS DE AMBIENTE PARA O PROJETO -------------------------------------------------------------

# esse nao e o melhor jeito de criar essas variaveis pois ainda podem ser apagadas sem querer pelo
# usuario. Idealmente deveriam ser variaveis de ambiente, mas o R nao permite objetos complexos
# para isso

.DB_SCHEMA <- new.env()

cat("* configurando schema do banco...")

with(.DB_SCHEMA, {

    library(dbrenovaveis)

    conn <- conectabucket("s3://ons-pem-historico", "hidro/meta-smap")

    subbacias <- new_tabela(
        nome = "subbacias",
        campos = list(
            new_campo("id", "inteiro"),
            new_campo("nome", "string"),
            new_campo("codigo", "string"),
            new_campo("bacia_smap", "string"),
            new_campo("bacia", "string"),
            new_campo("latitude", "float"),
            new_campo("longitude", "float"),
            new_campo("prod_se", "float"),
            new_campo("prod_sul", "float"),
            new_campo("prod_ne", "float"),
            new_campo("prod_n", "float"),
            new_campo("defasa_se", "inteiro"),
            new_campo("defasa_sul", "inteiro"),
            new_campo("defasa_ne", "inteiro"),
            new_campo("defasa_n", "inteiro")
        ),
        conexao = conn
    )

    modelos <- new_tabela(
        nome = "modelos",
        campos = list(
            new_campo("id", "inteiro"),
            new_campo("nome", "string"),
            new_campo("horizonte_previsao", "inteiro")
        ),
        conexao = conn
    )

    postosplu <- new_tabela(
        nome = "postosplu",
        campos = list(
            new_campo("id", "inteiro"),
            new_campo("id_subbacia", "inteiro", TRUE, subbacias, "id", "codigo", "subbacia"),
            new_campo("codigo", "string"),
            new_campo("peso", "float")
        ),
        conexao = conn
    )

    assimilacao <- new_tabela(
        nome = "assimilacao",
        campos = list(
            new_campo("id_subbacia", "inteiro", TRUE, subbacias, "id", "codigo", "subbacia"),
            new_campo("data", "data", alias = "janela"),
            new_campo("dia_assimilacao", "inteiro"),
            new_campo("rsolo", "float"),
            new_campo("rsub", "float"),
            new_campo("rsup", "float"),
            new_campo("rsup2", "float"),
            new_campo("peso_chuva", "float"),
            new_campo("tu", "float"),
            new_campo("eb", "float"),
            new_campo("sup", "float"),
            new_campo("qcalc", "float")
        ),
        conexao = conn
    )

    vazoes <- new_tabela(
        nome = "vazoes",
        campos = list(
            new_campo("id_subbacia", "inteiro", TRUE, subbacias, "id", "codigo", "subbacia"),
            new_campo("data", "data", alias = "janela"),
            new_campo("vazao", "float")
        ),
        conexao = conn
    )

    psat <- new_tabela(
        nome = "psat",
        campos = list(
            new_campo("id_subbacia", "inteiro", TRUE, subbacias, "id", "codigo", "subbacia"),
            new_campo("id_postoplu", "inteiro", TRUE, postosplu, "id", "codigo", "postoplu"),
            new_campo("data", "data", alias = "janela"),
            new_campo("precipitacao", "float")
        ),
        conexao = conn
    )

    previstos <- new_tabela(
        nome = "previstos",
        campos = list(
            new_campo("id_subbacia", "inteiro", TRUE, subbacias, "id", "codigo", "subbacia"),
            new_campo("id_modelo", "inteiro", TRUE, modelos, "id", "nome", "modelo"),
            new_campo("data_previsao", "data", alias = "janela"),
            new_campo("dia_previsao", "inteiro", alias = "horizonte"),
            new_campo("precipitacao", "float"),
            new_campo("vazao", "float")
        ),
        conexao = conn
    )
}, envir = .DB_SCHEMA, enclos = .DB_SCHEMA)

cat(" OK!\n")