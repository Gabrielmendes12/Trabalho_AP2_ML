# Trabalho_AP2_ML

⛽ Dashboard de Previsão de Vendas de Combustíveis

Este projeto consiste em uma aplicação web interativa desenvolvida em R e Shiny que analisa dados históricos de vendas de combustíveis no Brasil (1990-2025) e utiliza um modelo de Machine Learning (Regressão Linear) para prever tendências futuras de volume de vendas.

📋 Funcionalidades

O painel oferece as seguintes ferramentas de análise:

Filtros Dinâmicos: Seleção de dados por tipo de combustível (ex: Etanol, Gasolina, Diesel).

Visualização de Dados:

📈 Gráfico de Dispersão: Relação entre Ano e Volume de Vendas.

📊 Gráfico de Barras: Totalização de vendas por produto.

📦 Boxplot Interativo: Distribuição de vendas por região.

Previsão (Machine Learning): Interface para input de um ano futuro (ex: 2030) com retorno imediato da previsão de vendas baseada no modelo treinado.

Tabela de Dados: Visualização bruta dos dados filtrados com busca e paginação.

🛠️ Tecnologias Utilizadas

Linguagem: R

Framework Web: Shiny

Bibliotecas Principais:

ggplot2 & plotly: Para gráficos interativos e estáticos.

DT: Para tabelas de dados interativas.

stats: Para o modelo de Regressão Linear (lm).

📂 Estrutura do Projeto

├── vendas-combustiveis-m3-1990-2025.csv  # Dataset original (Fonte dos dados)
├── treinar_modelo.R                      # Script para processar dados e treinar a IA
├── app.R                                 # Código da aplicação Shiny (Interface e Servidor)
├── modelo_regressao_vendas.rds           # Arquivo binário do modelo treinado (Gerado pelo script)
└── README.md                             # Documentação do projeto


🚀 Como Executar

Para rodar este projeto localmente, siga os passos abaixo:

1. Pré-requisitos

Certifique-se de ter o R e o RStudio instalados. Instale os pacotes necessários executando o comando abaixo no console do R:

install.packages(c("shiny", "ggplot2", "plotly", "DT"))


2. Treinar o Modelo

Antes de iniciar o app, é necessário processar os dados e gerar o arquivo do modelo.

Abra o arquivo treinar_modelo.R.

Execute o script.

Verifique se o arquivo modelo_regressao_vendas.rds foi criado na pasta do projeto.

3. Rodar a Aplicação

Com o modelo gerado:

Abra o arquivo app.R.

Clique no botão "Run App" no RStudio ou execute:

shiny::runApp("app.R")


📊 Sobre os Dados

Os dados utilizados (vendas-combustiveis-m3-1990-2025.csv) contêm registros mensais de vendas de derivados de petróleo e biocombustíveis pelos distribuidores, discriminados por unidade da federação e produto.

Desenvolvido para fins de estudo em Análise de Dados e Machine Learning com R.
