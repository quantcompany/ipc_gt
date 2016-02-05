library(shiny)
library(rCharts)
library(forecast)
#library(ggplot2)

load("data/datos.RData")

month_table <- list("enero" = "01", 
                    "febrero" = "02",
                    "marzo" = "03",
                    "abril" = "04",
                    "mayo" = "05",
                    "junio" = "06",
                    "julio" = "07",
                    "agosto" = "08",
                    "septiembre" = "09",
                    "octubre" = "10",
                    "noviembre" = "11",
                    "diciembre" = "12")

month_table2 <- list("Ene" = "enero", 
                     "Feb" = "febrero",
                     "Mar" = "marzo",
                     "Abr" ="abril",
                     "May" = "mayo",
                     "Jun" = "junio",
                     "Jul" = "julio",
                     "Ago" = "agosto",
                     "Sep" = "septiembre",
                     "Oct" = "octubre",
                     "Nov" = "noviembre",
                     "Dic" = "diciembre")

regiones <- list("Región Nacional"=1,"Región.I"=2,"Región.II"=3,"Región.III"=4,
                 "Región.IV"=5, "Región.V"=6,"Región.VI"=7,"Región.VII"=8,"Región.VII"=9)

nombre_series <- names(principal)

meses <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

fechas <- c(paste0(meses,"-2011"),paste0(meses,"-2012"),paste0(meses,"-2013"),
            paste0(meses,"-2014"),paste0(meses,"-2015"))
fechas <- fechas[-(1:3)]

mesesf <- factor(substr(fechas,1,3))

adj_perc <- function(num){
    if(num >=0 & num < 2){
        out <- "leve"
    }else if(num < 4.5){
        out <- "moderada"
    }else{out <- "fuerte"}
    out
}
##==========Parrafos===================    

intro <- 'Esta es una aplicación interactiva para analizar los componentes del <b>Indice de Precios al Consumidor (IPC)</b> de Guatemala.<br>
<b>Tendencia-Ciclo</b>: o simplemente <i>Tendencia</i>Indica la marcha general y persistente del la serie observada, es el componente de la serie que refleja la evolución de largo plazo, este componente también 
recoge las oscilaciones periódicas de amplitud superior a un año: movimientos normalmente irregulares alrededor de la tendencia, en las que a diferencia de las variaciones estacionales, 
tiene un período y amplitud variables. <br>
<b>Estacionalidad</b>: es el movimiento periódico de corto plazo. Se trata de un componente causal debida a la influencia de ciertos fenómenos que se repiten de manera periódica en el periodo
de un año, por ejemplo estaciones, periodos de siembra y cosecha, etc. <br>
<b>Movimientos irregulares</b>: o aleatorios, son los movimientos erráticos que reflejan todos aquellos factores que influyen en el movimiento del índice y 
que son distintos de la tendencia general y a la la variación estacional, éstos no muestran ninguna regularidad, pueden darse debido a condiciones climáticas,
pérdida de cosechas, factores internacionales y otros fenómenos exógenos.<br>
El método utilizado es conocino como <b>"STL"</b> el cual es muy versátil y robusto para descomponer series de tiempo en sus respectivos componentes
STL es un acrónimo para <i>"Seasonal and Trend decomposition using Loess" </i>, éste método fue desarrollado por <a href="http://cs.wellesley.edu/~cs315/Papers/stl%20statistical%20model.pdf">Cleveland et al.
(1990)</a>.' 

creditos <- "Copyright (c) 2016, Quant Company. 
Visítenos en <a href ='http://www.quantcompany.com' 
target = _blank'>www.quantcompany.com</a> o bien en 
<a href ='https://twitter.com/quantcompany' target = _blank'>Twitter</a> 
o escribanos a afuentes@quantcompany.com para más información."


#Botones sociales ===============

facebook_script <- '<div id="fb-root"></div>
<script>(function(d, s, id) {
var js, fjs = d.getElementsByTagName(s)[0];
if (d.getElementById(id)) return;
js = d.createElement(s); js.id = id;
js.src = "//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.4";
fjs.parentNode.insertBefore(js, fjs);
}(document, \'script\', \'facebook-jssdk\'));</script>'

facebook_button <- '<div class="fb-share-button" data-href="https://internal.shinyapps.io/quantcompany/pobreza-en-guatemala/" data-layout="button_count"></div>'

FB_metas <- '<meta property="og:url"                content="https://internal.shinyapps.io/quantcompany/pobreza-en-guatemala/" />
<meta property="og:type"               content="article" />
<meta property="og:title"              content="Perspectivas de Pobreza en Guatemala 2014 - 2022" />
<meta property="og:description"        content="Explora de manera interactiva los indicadores de pobreza en Guatemala" />
<meta property="og:image"              content="http://quantcompany.com/wp-content/uploads/2015/10/pobrezaApp.png" />
'


twitter <- '<a href="https://twitter.com/share" class="twitter-share-button" data-url="https://quantcompany.shinyapps.io/pobreza-en-guatemala" data-via="quantcompany" data-lang="es">Twittear</a>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+\'://platform.twitter.com/widgets.js\';fjs.parentNode.insertBefore(js,fjs);}}(document, \'script\', \'twitter-wjs\');</script>'

disqus <- '<div id="disqus_thread"></div>
<script type="text/javascript">
/* * * CONFIGURATION VARIABLES * * */
var disqus_shortname = \'quantcompany\';
/* * * DON\'T EDIT BELOW THIS LINE * * */
(function() {
var dsq = document.createElement(\'script\'); dsq.type = \'text/javascript\'; dsq.async = true;
dsq.src = \'//\' + disqus_shortname + \'.disqus.com/embed.js\';
(document.getElementsByTagName(\'head\')[0] || document.getElementsByTagName(\'body\')[0]).appendChild(dsq);
})();
</script>
<noscript>Please enable JavaScript to view the <a href="https://disqus.com/?ref_noscript" rel="nofollow">comments powered by Disqus.</a></noscript>'
