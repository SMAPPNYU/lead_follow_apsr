###############################################################################
# 03-generating-html.r
###############################################################################

setwd("~/Dropbox/PAPERS/textasdata")
# loading relevant files
load("05-dashboard/qois.rdata")
load("05-dashboard/rs-tweets.rdata")
load("05-dashboard/media-rs-tweets.rdata")
#load("05-dashboard/rs-nyt.rdata")
load("05-dashboard/top-mcs.rdata")

K <- 100 #topics

for (k in 1:K){

## generating topic dropdown
topic.list <- scan("05-dashboard/topic-list.txt", what="character", sep="\n")
dropdown <- paste0("<option value=topic-", 1:K, ".html>", topic.list, "</option>")
dropdown[k] <- gsub('option ', 'option selected="selected" ', dropdown[k])

# generating lines with values of interest
qois1 <- paste0(
	"Topic usage by elites: ", sprintf('%0.2f', qois$prop[k]), "% all, ",
	'<font color="blue">', sprintf('%0.2f', qois$prop_sen_dems[k]), "% Senate Democrats</font>, ",
	'<font color="red">', sprintf('%0.2f', qois$prop_sen_reps[k]), "% Senate Republicans</font>, ",
	'<font color="blue">', sprintf('%0.2f', qois$prop_house_dems[k]), "% House Democrats</font>, ",
	'<font color="red">', sprintf('%0.2f', qois$prop_house_reps[k]), "% House Republicans</font>.</br>")
qois2 <- paste0('Top Members of Congress:',
	topmcs$text[topmcs$topic==k][1], ',',
	topmcs$text[topmcs$topic==k][2], ',',
	topmcs$text[topmcs$topic==k][3], ',',
	topmcs$text[topmcs$topic==k][4], ',',
	topmcs$text[topmcs$topic==k][5], '</br>')
qois3 <- paste0(
		"Topic usage by media and public: ", sprintf('%0.2f', qois$media[k]), "% Media, ",
		sprintf('%0.2f', qois$public[k]), "% informed public, ",
    sprintf('%0.2f', qois$random[k]), "% random users, ",
		'<font color="blue">', sprintf('%0.2f', qois$democrats[k]), "% Democratic supporters</font>, ",
	'<font color="red">', sprintf('%0.2f', qois$republicans[k]), "% Republican supporters</font>.</br>")

# image line
img.line <- paste0("<img src='img/words-plot-", k, ".png'>")

# data location
d.loc <- paste0('"data/ts-', k, '.csv", // path to CSV file')

# html text
html <- paste0(
'<html>
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<script type="text/javascript" src="js/dygraph-combined.js"></script>
<script src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
<link href="css/bootstrap.min.css" rel="stylesheet">

<style>
#sitemap { position: absolute; left: 50%; margin-left: -400px; top: 1%; margin-top: 50 px; }
#header1 { position: absolute; left: 50%; margin-left: -500px; top: 0%; margin-top: 30px; }
#div1 { position: absolute; left: 50%; margin-left: -500px; top: 0%; margin-top: 90px; }
#div2 { position: absolute; left: 50%; margin-left: 150; top: 0%; margin-top: 90px; }
#div3 { position: absolute; left: 50%; margin-left: -500; top: 0%; margin-top: 440px; }
#div4 { position: absolute; left: 50%; margin-left: -500; top: 0%; margin-top: 480px; }
#header2 { position: absolute; left: 50%; margin-left: -500px; top: 0%; margin-top: 550px; }
#div5 { position: absolute; left: 50%; margin-left: -500px; top: 0%; margin-top: 600px; }
#div6 { position: absolute; left: 50%; margin-left: 0; top: 0%; margin-top: 600px; }
#header3 { position: absolute; left: 50%; margin-left: -500px; top: 0%; margin-top: 1250px; }
#div7 { position: absolute; left: 50%; margin-left: -500px; top: 0%; margin-top: 1300px; }
#div8 { position: absolute; left: 50%; margin-left: 0; top: 0%; margin-top: 1300px; }
  body {
    padding-top: 60px; /* 60px to make the container go all the way to the bottom of the topbar */
  }
</style>
<link href="css/bootstrap-responsive.min.css" rel="stylesheet">

<body>

<div id="sitemap">
<b> &laquo; <a href="index.html">Back to Index</a>   &#8226;   Topic selection: </b>
<select onchange="if (this.value) window.location.href=this.value" class="input-xxlarge">
',
paste(dropdown, collapse="\n"),
'
</select>
</div>
<div id="header1"><h3>Topic Usage Over Time:</h3></div>
<div id="div1" style="width:600px">
</div>

<div id="div2" style="width:300px">
',
img.line,
'
</div>

<div id="div3">
<form class="form-inline">
<b>Display:  </b>
<label class="checkbox-inline">
	<input type="checkbox" id=0 onClick="change(this)" checked> Democrats
</label>
<label class="checkbox-inline">
	<input type="checkbox" id=1 onClick="change(this)" checked> Republicans
</label>
<label class="checkbox-inline">
	<input type="checkbox" id=2 onClick="change(this)" unchecked> Media
</label>
<label class="checkbox-inline">
	<input type="checkbox" id=3 onClick="change(this)" unchecked> Public
</label>
<label class="checkbox-inline">
	<input type="checkbox" id=4 onClick="change(this)" unchecked> Dem. Supporters
</label>
<label class="checkbox-inline">
	<input type="checkbox" id=5 onClick="change(this)" unchecked> Rep. Supporters
</label>
<label class="checkbox-inline">
  <input type="checkbox" id=6 onClick="change(this)" unchecked> Random Sample
</label>
       &#8226; <b> Smoothing period:</b>
    <input type="number" value="7" id="input" min="1" step="1" style = "width:50px;height:30px" onchange="changeRoll()">
    days 
</form>
</div>


<div id="div4">
',
qois1, qois2, qois3,
'
</div>

<div id="header2"><h3>Sample of representative tweets by Members of Congress:</h3></div>
<div id="div5" style="width:500px">
',
rs$embed[rs$topic==k][1],
rs$embed[rs$topic==k][3],
rs$embed[rs$topic==k][5],
'
</div>
<div id="div6" style="width:500px">
',
rs$embed[rs$topic==k][2],
rs$embed[rs$topic==k][4],
rs$embed[rs$topic==k][6],
'
</div>
<div id="header3"><h3>Sample of representative media tweets:</h3></div>
<div id="div7" style="width:500px">
',
media_rs$embed[media_rs$topic==k][1],
media_rs$embed[media_rs$topic==k][3],
media_rs$embed[media_rs$topic==k][5],
'
</div>
<div id="div8" style="width:500px">
',
media_rs$embed[media_rs$topic==k][2],
media_rs$embed[media_rs$topic==k][4],
media_rs$embed[media_rs$topic==k][6],
'
</div>
<br><br>
</div>

<script type="text/javascript">
  var g;
  g = new Dygraph(
    document.getElementById("div1"),
',
d.loc,
'
    {
      showRangeSelector: false,
      rollPeriod: 7,
      title: "",
      ylabel: "Pr(topic)",
      legend: "always",
      labelsDivStyles: { "textAlign": "right" , "background": "none"},
      colors: ["blue", "red", "black", "green", "darkblue", "darkred", "darkgray"],
      visibility: [true, true, false, false, false, false, false],
      labelsSeparateLines: true,
      labelsKMB: true,
    }          // options
  );

  function change(el) {
     g.setVisibility(el.id, el.checked);
  }
  function changeRoll(){
    value = input.value;
    g.adjustRoll(value);
  }

</script>
</body>
</html>
')

writeLines(html, con=paste0("05-dashboard/files/topic-", k, ".html"))


}