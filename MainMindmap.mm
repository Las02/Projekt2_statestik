<map version="freeplane 1.9.13">
<!--To view this file, download free mind mapping software Freeplane from https://www.freeplane.org -->
<node TEXT="Case2" FOLDED="false" ID="ID_696401721" CREATED="1610381621824" MODIFIED="1673717075696" STYLE="oval">
<font SIZE="18"/>
<hook NAME="MapStyle">
    <properties edgeColorConfiguration="#808080ff,#ff0000ff,#0000ffff,#00ff00ff,#ff00ffff,#00ffffff,#7c0000ff,#00007cff,#007c00ff,#7c007cff,#007c7cff,#7c7c00ff" show_icon_for_attributes="true" associatedTemplateLocation="template:/standard-1.6.mm" show_note_icons="true" fit_to_viewport="false"/>

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node" STYLE="oval" UNIFORM_SHAPE="true" VGAP_QUANTITY="24 pt">
<font SIZE="24"/>
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="default" ID="ID_271890427" ICON_SIZE="12 pt" COLOR="#000000" STYLE="fork">
<arrowlink SHAPE="CUBIC_CURVE" COLOR="#000000" WIDTH="2" TRANSPARENCY="200" DASH="" FONT_SIZE="9" FONT_FAMILY="SansSerif" DESTINATION="ID_271890427" STARTARROW="NONE" ENDARROW="DEFAULT"/>
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
<richcontent CONTENT-TYPE="plain/auto" TYPE="DETAILS"/>
<richcontent TYPE="NOTE" CONTENT-TYPE="plain/auto"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.attributes">
<font SIZE="9"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.note" COLOR="#000000" BACKGROUND_COLOR="#ffffff" TEXT_ALIGN="LEFT"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.selection" BACKGROUND_COLOR="#afd3f7" BORDER_COLOR_LIKE_EDGE="false" BORDER_COLOR="#afd3f7"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important" ID="ID_67550811">
<icon BUILTIN="yes"/>
<arrowlink COLOR="#003399" TRANSPARENCY="255" DESTINATION="ID_67550811"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000" STYLE="oval" SHAPE_HORIZONTAL_MARGIN="10 pt" SHAPE_VERTICAL_MARGIN="10 pt">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,5"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,6"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,7"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,8"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,9"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,10"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,11"/>
</stylenode>
</stylenode>
</map_styles>
</hook>
<hook NAME="AutomaticEdgeColor" COUNTER="5" RULE="ON_BRANCH_CREATION"/>
<node TEXT="Data-inspection" POSITION="right" ID="ID_1494631206" CREATED="1673617472449" MODIFIED="1673617477822">
<edge COLOR="#00ffff"/>
<node TEXT="relationship between temp and consumption" ID="ID_1031614445" CREATED="1673617494072" MODIFIED="1673619655872">
<node TEXT="" ID="ID_342003866" CREATED="1673619633213" MODIFIED="1673619633213"/>
</node>
<node TEXT="division in buildings" ID="ID_218523773" CREATED="1673619656560" MODIFIED="1673619679072"/>
</node>
<node TEXT="Minimal model" POSITION="right" ID="ID_977352092" CREATED="1671386364545" MODIFIED="1673617439513">
<edge COLOR="#ff0000"/>
<node TEXT="We tried to represent the physical law given" ID="ID_1991572290" CREATED="1673617439783" MODIFIED="1673617468553"/>
<node TEXT="outliers are removed - done by looking at the residuals plots until it looks okay" ID="ID_1866766480" CREATED="1673617468809" MODIFIED="1673620909647"/>
<node TEXT="conclusion" ID="ID_936032038" CREATED="1673619634761" MODIFIED="1673619761202">
<node TEXT="AIC is better for model with no climate data" ID="ID_1354970442" CREATED="1673619762251" MODIFIED="1673619831804">
<node TEXT="might be due to months data overshadowing the relevance of climate data" ID="ID_1452405410" CREATED="1673619815356" MODIFIED="1673619879349"/>
<node TEXT="climate data is however still relevant, but a simple model is always preferede" ID="ID_706462183" CREATED="1673619880167" MODIFIED="1673619901027"/>
<node TEXT="significant difference between the models when running an anova on no cilamte model and model contaning hum and wind-spd" ID="ID_1107654225" CREATED="1673619902651" MODIFIED="1673619957812"/>
<node TEXT="result: we choose the simple model as it is statistically more appropriat to use, but will still present the model containing climate data, but just say that AIC is decreased so much when adding climate so it woudl not make sense to choose this for further investigation" ID="ID_1294068016" CREATED="1673619960488" MODIFIED="1673620232863"><richcontent TYPE="NOTE" CONTENT-TYPE="xml/">
<html>
  <head>
    
  </head>
  <body>
    <pre tabindex="0" aria-label="Console Output" role="document" class="GND-IWGDH3B" id="rstudio_console_output" style="font-family: DejaVu Sans Mono, monospace; font-size: 13.3333px; border-top-style: none; border-top-width: medium; border-right-style: none; border-right-width: medium; border-bottom-style: none; border-bottom-width: medium; border-left-style: none; border-left-width: medium; margin-top: 0px; margin-right: 0px; margin-bottom: 0px; margin-left: 0px; white-space: pre-wrap !important; line-height: 1.2; color: rgb(255, 255, 255); font-style: normal; font-weight: 400; letter-spacing: normal; text-align: -webkit-left; text-indent: 0px; text-transform: none; word-spacing: 0px; background-color: rgb(0, 34, 64)">AIC table of simple model vs. climate model (hum and wind.spd)

<span class="GND-IWGDH3B" style="border-top-style: none; border-top-width: medium; border-right-style: none; border-right-width: medium; border-bottom-style: none; border-bottom-width: medium; border-left-style: none; border-left-width: medium; margin-top: 0px; margin-right: 0px; margin-bottom: 0px; margin-left: 0px; white-space: pre-wrap !important">\begin{table}[ht]
\centering
\begin{tabular}{rrr}
  \hline
 &amp; df &amp; AIC \\ 
  \hline
new\_fit &amp; 169.00 &amp; -995.04 \\ 
  fit\_clima &amp; 177.00 &amp; -1743.11 \\ 
   \hline
\end{tabular}
\end{table}</span></pre>
  </body>
</html></richcontent>
</node>
</node>
<node TEXT="new conclusion: brug fit_clima (better) AIC is not absoulute" ID="ID_788233639" CREATED="1673621327938" MODIFIED="1673621344279"/>
</node>
</node>
</node>
</map>
