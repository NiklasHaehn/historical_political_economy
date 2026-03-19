---
title: "The Causal Effect of Special Elections on Media Internal Pluralism: A Historical Investigation"
subtitle: "POLI 228 -- Historical Political Economy"
author:
  - name: Niklas Haehn
date: "March 18, 2026"
abstract: |
  This paper investigates the causal effect of electoral campaign intensity on the
  internal pluralism of newspaper coverage using a stacked difference-in-differences
  design. Exploiting deaths of U.S. House members as exogenous shocks to the political
  agenda, I analyze over 1.3 million newspaper articles from six outlets across
  100 years of American history. 
  Firstly, I find that special election campaigns do not increase the number of political articles published.
  Secondly, I find that special election campaigns increase partisan slant in
  headline coverage, with no evidenz for specific effects over time.
  These results suggest that electoral competition reduces the internal pluralism
  of political information available to voters, with implications for theories of democratic
  accountability and media quality.
  However, the causal effect of pecial election campaigns increase partisan slant is not robust
  against other specificaations such as Donut Difference-in-Difference Designs.
keywords: "media pluralism, special elections, difference-in-differences, partisan bias, political polarization"
---

# Introduction

Classical models of political accountability are based on the assumption
of full, or at least sufficiently rich, information [@downs1957; @fiorina1978; @ashworth2017]. Voters, however, do not perceive politics
directly, but rather through the mediation of the media [@zaller1992; @zaller1999; @iyengar2010; @mccombs1972]. I argue that the
quality of the media environment feeds back into the quality of the
information available to voters and thus makes accountability a function
of media quality [@snyder2010; @stromberg2004a]. Changes in
the media environment may therefore constitute part of the explanation
for democratic erosion as well as rising political polarization over
time [@prior2007; @lelkes2017].
Whereas the traditional accountability literature primarily emphasizes the availability of political information to voters [@ashworth2012; @stromberg2015],
the quality of that information has received comparatively less attention and has only recently become central to debates on fake news and misinformation [@allcott2017].
With this paper, I do not contribute to the debate on false information; rather, I
define media quality as partisan bias in news coverage [@gentzkow2010].


The quality of information is especially important in the period before
an election, when voters make their electoral decisions. In line with @gelman1993 , I argue that the campaign period is a time of
heightened political intensity [@lazarsfeld2021; @campbell1960; @zaller1992] in which, on the one hand, voters
have a particularly strong demand for political information that aligns
with their prior political views [@stroud2011]. At the same time,
election campaigns generate a particularly high supply of political
information [@iyengar2000] produced by parties and political
elites, which is then taken up by the media [@druckman2013]. Together, these dynamics cause the quality of media
coverage to decline during campaign periods, conditional on the level of
political intensity.


For this reason, this paper asks what effect political competition
intensity has on media coverage. For this study, I focus on the United
States because of its pronounced and relatively stable media market in
one of the world's oldest democracies [@snyder2010; @petrova2011]. I adopt a historical political economy perspective and
examine a period spanning 100 years of U.S. history. This long time
horizon allows me to identify a sufficient number of special elections,
which I use as an exogenous shock to the media agenda in order to
identify causal effects [@mccombs1972].


This study makes several contributions. First, to the best of my
knowledge, it is the first study to systematically compile historical
data on special elections over the past 100 years. I then show the
causal effect of campaign intensity on media behavior, first through the
sheer number of political stories and, in a second step, through media
quality. The paper thus contributes to the literature on media quality
and journalistic norms in historical perspective. In a broader context,
it also contributes to our understanding of democratic backsliding and
political polarization.

This study proceeds as follows. I review existing theories
on the effect of political intensity on domestic media
pluralism, identify several political-economic micro-mechanisms, and
derive three hypotheses based on this analysis. I test these
hypotheses using various descriptive statistics, which
provide insight into the time-series data, as well as, in particular, 
event studies and difference-in-difference models. 
I show that while special election campaigns do not effect the number of political articles published,
special election campaigns have a positive effect on the partisan slant of the newspaper articles.
Furthermore, I find no evidence for time conditional effect of special election campaigns.
However, in robustness tests, I show that the effect is not robust in more demanding models such as a donut-difference-in-difference models.
Finally, I discuss the results, highlight limitations and outline further research in the field.

# Theorie

## Medien, Funktion und Pluralismus
Media report selectively and, in doing so, structure both public opinion
and the political agenda. From this perspective, the media are not
merely transmitters, but political institutions [@cook2005] whose
organizational forms and market logics vary systematically [@baumgartner2009]. In their comparative analysis of media systems, @hallin2004 identify differences along the dimensions of
political parallelism, journalistic professionalism, state intervention,
and market structures. In systems with high political parallelism,
pluralism is often organized through differentiation across outlets,
while a more coherent line tends to dominate within individual media
organizations; in more professionalized or public-service-oriented
environments, pluralism is more likely to be institutionalized within
individual media outlets [@hallin2004; @blumler1995; @chadwick2017].


<!-- Pluralismus -->
The political economy literature points to a trade-off between internal
pluralism and pluralism across media outlets [@mullainathan; @gentzkow2010]. In media markets with strongly
competitive structures, and the correspondingly higher degree of
pluralism across outlets that often accompanies them, the internal
pluralism of content tends to decline [@mullainathan; @gentzkow2006; @gentzkow2010]. Outlets differentiate themselves as
brands or niches; internal heterogeneity weakens branding, and internal
pluralism therefore declines. In intense campaigns, competition for
attention increases, so this effect should become even stronger. An
exception in such media markets may be state media, which typically
carry a political mandate to maintain internal pluralism [@hallin2004].


<!-- Argument -->
I argue that the internal pluralism of media is of decisive importance
for the accountability of political elites and parties. @stroud2011
show that voters strongly self-select into media outlets on the basis of
their partisanship [@prior2007; @iyengar2009]. Pluralistic media consumption is therefore rare.
Newspapers often emerge from a tradition of strong ties to political
parties [@kaplan2003; @hallin2004] and, for this reason,
already tend to exhibit lower internal pluralism than other media forms,
in which, traditionally, broadcast licenses were more limited and the
state intervened more strongly in the media market [@hallin2004]. The U.S. media market is known for its limited state intervention
and strong competition [@hallin2004; @hamilton2004]. For
this reason, I argue that, especially in the United States, the
pluralism of the information voters consume depends on the internal
pluralism of the media.

## Mechanismen

Media behavior is shaped to a large extent by three external forces:
state intervention, professional journalistic norms, and economic
constraints [@hallin2004; @cook2005; @hamilton2004]. I argue that, by conducting a case study
within a single democratic country, I hold the first two factors
constant by design, so that media behavior is driven primarily by
economic constraints.

### Demand Side

Campaigns increase identity salience and emotional involvement; as a
result, motivated reasoning intensifies and the demand for congenial
information rises [@taber2006], while media outlets respond in
profit- and audience-maximizing ways by adopting a more coherent "house
line." Voters prefer news that is consistent with their affectively
grounded attitudes in order to reduce cognitive dissonance, and they
search for and interpret information selectively [@lodge2013; @gentzkow2010]. @huddy2015 show that campaign
events activate strong emotions tied to partisan identity, thereby
reinforcing the effect of affectively grounded attitudes. This makes
social sorting on the basis of partisanship more likely during periods
of high salience [@mason2016; @mason2018; @iyengar2009; @messing2014].

Media are economic actors and seek, above all, to maximize their
audience and, with it, their profits. @gentzkow2010 show
that outlets respond systematically to consumer demand, while
@mullainathan model confirmatory preferences and show
that competition can shift an outlet's slant toward the prior beliefs of
its readers [@hamilton2004].

### Supply Side

Campaigns constantly produce "ready-made" inputs such as events, PR, and
talking points. Under time pressure and tight production cycles,
newsrooms rely on standardized templates such as the horse-race frame,
strategy/game framing, and scandal coverage [@patterson2011; @cappella1997; @aalberg2012]; this crowds
out more heterogeneous and time-intensive perspectives. Political actors
have different strategic means of shaping the media agenda, the most
influential of which are framing [@aalberg2012; @vreese2005], agenda-setting [@mccombs1972; @mccombs2021], and priming [@iyengar2010].
During campaigns, messages within political camps converge and become
more tightly coordinated [@zaller1992; @druckman2013].

Due to resource constraints in news production, above all limited time,
media rely on these prefabricated messages provided by political actors
[@gans2004; @shoemaker2009]. This effect is further intensified
during campaign periods because the demand for political news rises,
while the volume and complexity of political events also increase
[@shoemaker2009; @gans2004].

### Strategic Alliances

When access to candidates, surrogates, or embargoed information is
central, access itself can function as a disciplinary instrument; media
internalize this logic through self-censorship, source dependence, and
indexing. @bennett1990 describes how the range of legitimate views in
news discourse is often "indexed" to official or elite debate; when
elites control access, this narrows the range of views that enters
coverage. This effect is further reinforced by journalistic routines
governing the use of official sources, with the result that dissenting
voices are screened out [@bennett2008; @gans2004; @cottle2000].

Competing theories instead assume that the increased demand for
information during elections, uncertainty about the winner and the
future policy direction [@huddy2015; @gentzkow2006; @downs1957], and also the stronger scrutiny of the media by watchdog
institutions alongside self-censorship rooted in professional role
conceptions [@tuchman1972; @schudson2001] lead to higher internal
pluralism. In addition, this literature argues that outlets become less
dependent on their core audience [@fowler2022] and have more material available for their coverage [@dimitrova2012].



Based on that I derive three hypotheses:

*H1:* Campaign periods  increase the demand for political news and therefore increase the number of articles published by the outlets.

*H2:* Campaign periods reduce the internal pluralism of media coverage.

*H3:* The negative effects of campaign intensity increase over time as
audience polarization, and thus demand-side polarization, rises.

# Design

The analysis of media effects and effects on the media is subject to the
fundamental problem of endogeneity. The literature has extensively
described the influence of the media on the political process [@mccombs1972; @iyengar2010]. These effects primarily include
agenda-setting, priming, and framing. From a causal perspective, the
question is therefore whether pluralism in the media declines as a
result of campaign intensity, or whether campaign intensity increases as
a result of strongly partisan media. To address this endogeneity problem
[@snyder2010; @gerber2009a], I use
special elections as shocks to the political agenda. I argue that, under
certain conditions, special elections can satisfy the requirements of an
exogenous shock.

## Special Elections as Exogenous Schock

Special elections are elections that are called, or may be called, when
a seat in a U.S. legislature becomes vacant in order to fill it. In
general, there is substantial variation in the institutional rules
governing how a vacant seat is filled. In this study, I focus on the
House of Representatives, because the Constitution has long stipulated
that vacant seats may not be filled by appointment, but must instead be
filled through an election. A special election in response to a vacant
seat is called by the governor of the respective state. When a seat
becomes vacant close to the end of the term, however, no special
election is often held, and the seat is instead filled at the next
general election.

<!-- Verschiedene Ursachen einer Special election -->
I argue that the occurrence of a special election is not, by itself,
sufficient for it to serve as an exogenous shock. In principle, there is
substantial variation in the reasons why a seat becomes vacant. The most
common cases are resignation and death while in office. The reasons for
resignation can be highly diverse, and historically it is often
difficult to reconstruct the full motivations of politicians. I discuss
three main reasons here: resignations due to political scandals,
resignations for other political offices, and strategic resignations.

### Resignations

Political scandals pose a problem for the operationalization of special
elections as an exogenous shock, because they violate the "no
co-treatment" assumption that is fundamental to causal identification.
In such cases, the media are not concerned only with the election
campaign itself, but also with the scandal that prompted the
politician's resignation.

Resignations for other political offices violate the assumption that the
shock is unforeseen, and thus exogenous. Most politicians who run for
another office, such as governor, senator, or a position in the federal
administration, do not do so randomly, but instead prepare themselves
and their political environment for this transition, which in such cases
unfolds in a planned and structured manner. The same applies to
strategic resignations, for example when a party uses a moment of high
public support to engineer a generational transition rather than waiting
for the next election.
Overall, I argue that resignations are generally not suitable as exogenous shocks.

### Death

Compared to resignations, deaths in office constitute a far more
unforeseen shock to the political agenda. Yet even here, it is necessary
to distinguish the manner in which the officeholder died. In the case of
a person with a known illness, death is less unforeseen than in the case
of someone who dies as a result of external violence, such as an
accident or physical attack.
Figure 1 shows the distribution of causes of death by category for people who died while serving in the House.
As can be seen, the most common cause is a sudden illness. 
An illness is considered sudden if it was not chronic and was therefore not known before the person took office.
As a rule, people died from these illnesses within six weeks.
The second most common category, with 83 cases, consists of individuals whose cause of death is unclear or unknown.
This is followed by the category of chronic illnesses; accidents and violence represent a small minority compared to illnesses.
In total, I identified 246 cases in which a special election was held due to the death of a House member; in 163 of these cases, I was able to identify and classify the cause of death.

![Distribution of causes of death for members of Congress who died in office, 1900--2020. Illness (fast) constitutes the primary identification category.](figures/descriptives/06_plot_01_deaths_cause.png){width=90%}

## Data

To test the derived hypotheses, I construct an original dataset that
links special elections to media behavior. The dataset I compile is
structured at the week-by-newspaper-outlet level. The observation period
for special elections begins with the First Congress in 1789 and extends
to the 2020 election. The observation period for the newspaper analysis
is shorter due to data availability and covers the period from 1900 to
2020.

### Special election Data

As the data foundation for special elections, I merge datasets on
members of Congress with datasets on special elections. The primary data
source is the *Biographical Directory of the United States Congress*
(Bioguide). The dataset is structured at the legislator-by-Congress
level and includes information on the beginning and end of
representatives' terms, as well as, in cases where a term ends early,
the reason for that early termination. However, the recorded reason for
the end of a term is fairly coarse and usually contains only information
on whether the individual died or resigned. In addition, the dataset
includes a biographical text with further information on each
legislator. One challenge in the data is that the beginning of the term
does not necessarily correspond to the time at which legislators were
sworn in.

I combine the individual congressional data with data on special
elections. Existing datasets on special elections, such as those by
@hirano2009 , cover only a fraction of the full observation
period. The only comprehensive dataset on special elections that extends
back to the First Congress is available on Wikipedia. Although the data
quality of Wikipedia may reasonably be questioned, I justify its use in
this project because no better source exists for the full period. I
validate these data using information from the notes contained in
Bioguide.

Information on causes of death also comes from Wikipedia. Wikipedia in
turn lists *Memorial Addresses* from HathiTrust as the primary source.
Again, I use the Bioguide information available to me to verify the
recorded causes of death. I then classify causes of death by means of
keyword matching into the categories illness (fast), illness (slow),
accident, violence, and unclear/unknown. In later analyses, I
distinguish between these different forms of death as a robustness test.

I merge these datasets into a joint special election dataset that
records, for each special election, who left Congress and who was
elected as a replacement, including the relevant biographical
information, the reason the special election occurred, and, if the
original member of Congress died, the cause of death.

![Number of special elections by cause of vacancy over time (1789--2020).](figures/descriptives/06_plot_02_special_elections_over_time.png){width=90%}

Figure 2 displays the number of special elections by decade. 
The figure makes clear that special elections are not uniformly distributed over time. Rather, their frequency reflects historical and institutional variation, including changes in mortality, resignation patterns, political career incentives, and state-specific rules for filling House vacancies. 
Vacancies can arise for different reasons and because the timing of replacement elections is regulated by state law, I expect regional variation.

Figure 3 shows the distribution of special elections by U.S. region over the full observation period. 
This figure is confounded by the fact that states joined the United States at different historical moments and therefore are not observed for the same length of time. Earlier-admitted states, particularly in the Northeast and parts of the South, mechanically have more opportunities to experience special elections than later-admitted states in the West. 
Figure 4 shows the distribution of causes of death among House members by region over the entire observation period.
As can be seen, the pattern from Figure 3 carries over to Figure 4; here, too, the number of causes of death is confounded by the time when the states were admitted. It is important to note that there does not appear to be any sorting of causes of death, e.g., due to regional disease outbreaks or political violence.


![Geographic distribution of special elections by U.S. region (1789--2020).](figures/maps/05_map_region.png){width=75%}

![Geographic distribution of special elections by region and cause of death.](figures/maps/05_map_region_cause.png){width=90%}

### Newspaper Data

The newspaper data come from ProQuest TDM Studio. TDM Studio provides
access to a large number of newspaper archives, including historical
newspapers collected and curated by ProQuest itself. For my research
design, I consider only U.S. newspaper outlets for which historical
newspaper articles are available. Given computational constraints, while
still aiming to ensure geographic coverage, I select representative
outlets from each region of the United States. To the extent that a
newspaper's political orientation does not affect its internal
pluralism, the specific choice of outlet is of lesser importance. I
focus on newspapers with a long tradition in their respective regions
and a reputation for journalistic standards, so that no obvious biases
are introduced into my models.

For the Northeast, I use historical newspaper articles from the *New
York Times*. For the South, I draw on *The Atlanta Constitution* and
*The Baltimore Sun*. For the Midwest, I use historical articles from the
*Chicago Tribune*, and for the West, I use the *Los Angeles Times* and
the *Los Angeles Sentinel*.

To make the number of articles manageable, I restrict the sample to
English-language articles classified as "news" and containing political
keywords such as *Congress*, *election*, or *campaign* in the full text.
After filtering, the dataset contains 1,347,370 articles.

To measure partisan tone, I use the large language model Llama 3.2, a 3-billion-parameter open-source model, to classify the partisan slant of each article headline. For each headline, the model is prompted to assign a directional slant score: positive values indicate Republican-leaning coverage, negative values indicate Democratic-leaning coverage, and zero indicates neutral framing. The prompt instructs the model to evaluate whether the headline implicitly favors one party's positions, candidates, or narrative frame over the other's.

The resulting headline-level slant scores are then aggregated to the outlet-week level by computing the mean score across all articles published by a given outlet in a given week. This outlet-week mean constitutes the primary outcome variable, *slant*, used in the event-study and difference-in-differences specifications. As a secondary outcome measuring the *extremeness* of coverage rather than its direction I use the absolute value of each headline-level score, $|\text{slant}|$, averaged within outlet-weeks. A higher absolute score indicates that articles cluster at one partisan extreme, regardless of direction, and thus captures internal homogeneity of coverage.

This measurement approach has several advantages. First, it scales to the full corpus of 1.3 million articles without requiring manual coding. Second, because the model evaluates headlines rather than full article text, it focuses on the most salient editorial signal visible to readers. Third, aggregating to the outlet-week level smooths idiosyncratic noise in individual headline classifications and aligns with the panel structure of the design. A limitation is that LLM-based classification may introduce systematic errors for historically distant language; I address this by reporting results separately for pre- and post-1970 subsamples.

## Identification Strategy and Threats to Identification

For identification, I employ a stacked difference-in-differences (DiD) design combined with event-study plots to assess dynamic treatment effects [@angrist2008; @roth2023]. The stacked DiD approach constructs a separate clean comparison dataset for each event (i.e., each special election), in which the treated region is matched against all not-yet-treated or never-treated control regions observed in the same event window. These event-specific datasets are then stacked and estimated jointly. This avoids the negative-weighting problem that arises in two-way fixed-effects estimators with heterogeneous treatment timing [@roth2023].

The estimating equation takes the form:

$$Y_{irt} = \sum_{\tau \neq -1} \beta_\tau \cdot \mathbf{1}[\text{EventTime}_{it} = \tau] \times \text{Treated}_{ir} + \delta_i + \gamma_{t} + \varepsilon_{irt},$$

where $Y_{irt}$ is the outcome (partisan slant or article volume) for outlet $i$ in event-specific stack $r$ at relative time $t$. $\text{Treated}_{ir}$ equals one if outlet $i$ is located in the region where event $r$ took place. $\delta_i$ are event fixed effects and $\gamma_t$ are event-time fixed effects, both estimated within each event stack. Standard errors are clustered at the event level to account for within-event correlation across outlets and weeks.

The coefficients $\beta_\tau$ for $\tau < -1$ constitute a pre-trend test: under the parallel-trends assumption, these should be jointly indistinguishable from zero. The post-period coefficients ($\tau \geq 0$) capture the dynamic treatment effect over the campaign window. The pooled average treatment effect on the treated (ATT) is computed as the weighted average of post-period $\beta_\tau$ estimates.

I use the stacked DiD rather than a simple pooled two-way fixed-effects specification for two reasons. First, the stacked design explicitly accounts for the fact that control units may themselves be treated in other event stacks, which would contaminate a naive estimator. Second, restricting each comparison to a well-defined event window of 25 weeks (12 pre-treatment + 1 reference + 12 post-treatment) for the volume analysis (*H1*) and 13 weeks for the tone analysis (*H2* and *H3*) respectively, prevents contamination across temporally overlapping events.

Because I do not observe newspaper circulation directly, the geographic reach of each outlet can only be proxied. I therefore assign outlets to U.S. regions as the coarsest feasible level of geographic clustering. A region is treated if it is in a vacancy period preceding a special election. The remaining regions in the same time period serve as the control group.

The parallel-trends assumption is plausible given the exogenous nature of the treatment: the timing of a House member's death is not strategically chosen by either the outlet or political actors. During the campaign period, there may be additional influences on the media environment, such as political advertising by parties, candidates, or interest groups [@bode2016], but these are all causally downstream of the vacancy event and therefore form part of the reduced-form effect I seek to estimate.

To assess the robustness of the parallel-trends assumption beyond the visual pre-trend test, I apply the sensitivity analysis of @roth2023 (HonestDiD). This method characterizes the minimum deviation from parallel trends, expressed as a multiple $\bar{M}$ of the largest observed pre-trend, that would be required to overturn the post-period estimates. Results of this analysis for each main specification are presented in the appendix.

Figure 5 displays vacancy periods and the timing of special elections across regions over the full observation period and is intended to illustrate the research design. Each blue-shaded interval denotes a vacancy period preceding a special election, which is indicated by a red vertical line. In the empirical framework outlined here, this vacancy period defines the treatment period.
The 12 weeks prior to the onset of the vacancy period are used as the pre-treatment window. Regions that are not treated during a given period serve as the control group for both the difference-in-differences specification and the event-study design.


![Vacancy periods and special elections by U.S. region, 1920 - 2020 .](figures/descriptives/06_plot_06a_region_timeline_1920_2020.png){width=90%}

# Results

## Article Volume

I first examine whether a special election affects the *volume* of political news coverage, that is, the number of articles published per outlet-week (*H1*).  If the mechanism is correct, that political campaign periods increase the demand of political news, we should see in increase in the number of political articles. 
Figure 7 shows the full event-study trajectories for article volume in the pooled and post-1970 samples. 
I distinguish between the pooled sample and the post-1970 subsample because the literature identifies the early 1970s as the beginning of a period of rising partisan polarization in American public opinion [@highton2011; @layman2006]. If audience polarization amplifies demand-side pressure on outlets to adopt partisan framing, then treatment effects should be larger in the post-1970 period.
Pre-period coefficients are flat and jointly insignificant, supporting the parallel-trends assumption. Post-period coefficients are also close to zero and statistically indistinguishable from zero throughout the campaign window. This null result is consistent with the view that special elections shift the partisan framing of coverage without expanding its total volume.

![Event study for article volume. Left: pooled sample; right: post-1970 subsample. Error bars show 95% CI clustered by event.](figures/analysis/08_fig_art_es.png){width=90%}

Figure 8 displays the ATT estimates for article volume across the pooled sample, the post-1970 subsample, and the exogenous-deaths subsample. None of the specifications shows a significant effect of the campaign periode on the political articles volume. This indicates that even if the described mechanism is true, the newspapers do not respond to the increase in the demand of political news by a higher supply. Model three, the Exogenous deaths model even suggest that the effect might be negativ. However, I fail to find support for *H1* and therefore reject the hypotheses.

![ATT estimates for article volume (n\_articles). Stacked DiD, 95% CI clustered by event. None of the specifications yield a statistically significant effect.](figures/analysis/08_fig_art_att.png){width=75%}

## Partisan Tone

Having established a null effect on article volume, I turn to partisan tone. Figure 9 shows ATT estimates for headline slant across four specifications: the pooled sample, the post-1970 subsample, the exogenous-deaths subsample (fast illness, accidents, and violence combined), and the illness-slow subsample as a placebo comparison. The first two specifications represent the primary estimates; the exogenous-deaths specification provides the cleanest causal identification by restricting to the most unforeseen causes of vacancy; the illness-slow specification serves as a within-deaths placebo, since slow illnesses may violate the exogeneity assumption due to anticipation effects, so that I cannot rule out endogeneity problems.

All the models presentetd are significant at the 95% confidence level. 
The first two models show an average treatment effect of 0.03 on the outcome variable.
This means that being in a political campaign phase is associated with an increase in party political orientation of 0.03 points,
where party political orientation ranges from 0 to 1, with 1 representing full party affiliation and 0 representing complete non-partisanship.
Furthermore, the lack of a significant difference between the pooled and the post-1970 models suggests that there are no substantial differences between the periods before and after 1970.
The third model shows an effect that is twice as strong when I consider only exogenous deaths. This is strong empirical evidence that resignations can also be strategically motivated and thus lack a competitive aspect.
Finally, illnesses (slow) are shown in gray as a placebo test. With 0.09 points and relatively small confidence intervals compared to the other models, these election campaigns show the strongest effect.
However, the values should be interpreted with caution, as this model assumes no exogeneity and that the media anticipated the death and the special election campaign. The model does, however, provide the descriptive finding that the media react more biasedly when they can foresee a special election campaign.
This model provides strong support for *H2*, which posits that political campaigns have a negative effect on internal media pluralism.
Furthermore, there is evidence against *H3*, which suggests that the effect is conditional on societal polarization.
At least in the pooled model, there are no visible signs that would suggest different patterns between the pooled and post-1970 models. 
A pre-1970 model could not be estimated reliably due to the small number of observations and the lack of pre-trends.


![ATT estimates for partisan slant. Stacked DiD, 95% CI clustered by event. Blue indicates exogenous specifications (pooled, post-1970, exogenous deaths); grey indicates the illness-slow placebo. Filled circles denote statistical significance at the 5% level.](figures/analysis/08_fig_slant_att.png){width=75%}

Figure 10 presents the full event-study trajectories for the three main slant specifications, the pooled model acorss the full observation periode. Pre-period coefficients are mostly flat across, providing visual support for the parallel-trends assumption. 
Interestingly, it is precisely the exogenous model that appears to have a pre-trend period. This is not surprising, since in cases of sudden, fatal illnesses, the media typically report on the severity of the illness as early as the week before death.
It might therefore be worthwhile to extend the reference period even further back in time using a donut design.
Post-period coefficients turn positive at the onset of the campaign window and remain elevated, consistent with a sustained increase in partisan slant during special-election campaigns. 
It is important to note that the coefficients flatten out again after a short period of time, which in the pooled model could be evidence of confounding factors such as political scandals.

![Event study across slant specifications. Pooled, post-1970, and exogenous-deaths estimates overlaid. Error bars show 95% CI clustered by event.](figures/analysis/08_fig_slant_multi.png){width=85%}

## Internal Pluralism

The following results are exploratory given the data limitations documented in the preceding section. Due to limited outlet coverage in the article-level data, pre-trend tests fail across all specifications, and results should be interpreted descriptively rather than causally.

Figure 11 presents event-study estimates for the article-level slant outcome using a stacked DiD with outlet $\times$ event fixed effects. 
To examine whether editorial vacancies affect the directional slant and extremeness of individual articles, I construct an article-level stacked panel in which each article appears once per event that covers its publication week. I estimate two-way fixed effects models with *headline slant* and *absolute headline slant* as outcomes. The preferred specification absorbs outlet-by-event fixed effects (outlet × event FE), which eliminates any outlet-specific baseline that may differ across events and identifies the treatment effect purely from within-outlet, within-event variation over time. As a robustness check, I also estimate a model with additive fixed effects for events, outlets, and time periods separately (additive FE), which is less conservative but assumes that outlet-specific baselines are constant across events.
The plot reveals two interesting patterns. First, the more strictly conservative model—which aligns more closely with my concept of internal media pluralism—shows clearer and stronger effects, providing strong empirical support for *H2*.
Second, even though the difference between the models is not statistically significant, the bottom two models reveal a pattern in which the effects diminish after 1990. This pattern runs counter to my expectation from *H3*. There may be various reasons for this pattern after 1990, such as an increase in journalistic standards or a stronger focus on the core readership due to fierce competition from other media forms like the internet and social media.


![Event study: article-level slant as outcome. Five specifications differing in sample and fixed-effect structure. Blue (Full, outlet $\times$ event FE) is the preferred specification; grey lines show alternatives. Error bars show 95% CI clustered by event.](figures/analysis/12_fig_article_slant.png){width=90%}

# Robustness

A key concern in event-study designs is that estimated pre-treatment coefficients may partly reflect anticipation effects rather than a stable baseline. In the context of electoral vacancies caused by illness, journalists and editors may begin adjusting coverage already before the formal vacancy is declared, for instance, as a member's health deteriorates and the prospect of a special election becomes foreseeable. To address this concern, I implement a *donut difference-in-differences* design, which removes the five weeks immediately surrounding the treatment window (event time $-2$ through $+2$) and shifts the reference period to $t = -3$. This creates a "donut hole" around the transition period, ensuring that identification relies only on variation from weeks further removed from the vacancy start.

Figures 12 and Figures 13 illustrate the event-study coefficients under this specification. The grey shaded band marks the excluded time periode. For article volume, the coefficients in the retained pre-period show no systematic pre-trend, and the post-period estimates cannot be distinguised from zero in all specifications. For partisan slant, the event-study plot in Figure 13 shows broadly flat pre-trends for the pooled and exogenous-deaths specifications, with post-period coefficients shifting below zero, indicating a negative effect which is not consistent with the main results. The post-1970 sub-sample is the only model which shows no signficant effect in this robustness specification.

![Event study estimates for article volume under the donut design. The grey shaded area marks the excluded period ($t \in \{-2,\ldots,2\}$); the reference period is $t = -3$. Left panel: pooled specification. Right panel: post-1970 subsample.](figures/analysis/13_fig_donut_art_es.png){width=90%}

![Event study estimates for partisan slant under the donut design, for three specifications. Colors and linetypes distinguish specifications as in the main analysis.](figures/analysis/13_fig_donut_slant_es.png){width=90%}

Figure 14 presents the pooled average treatment effects on the treated (ATT) across all specifications and both outcomes under the donut design. For article volume, the point estimates are negative but imprecisely estimated and not statistically significant at conventional levels. For partisan slant, the pooled and exogenous-deaths specifications retain a negative and statistically significant ATT; the effect sizes are even larger than in the main analysis. The post-1970 estimate is close to zero and insignificant. Taken together, the donut results suggest that the slant effects identified in the main analysis are  primarily driven by the transition period itself, and not robust for the whole treatment periode.

![Pooled ATT estimates under the donut design for article volume (left) and partisan slant (right). Filled circles indicate $p < 0.05$; open circles indicate $p \geq 0.05$. Error bars show 95% confidence intervals.](figures/analysis/13_fig_donut_att.png){width=90%}

# Discussion
In this study, I examined the question of how special election campaigns in the United States affect the behavior of print media. To do so, I constructed an original dataset covering all House special elections from 1789 to 2020, including, where available, information on the causes of death of House members. I then merged this dataset with data on historical newspapers and their publications.

Using several difference-in-differences models, I find causal evidence that special election campaigns have a negative effect on the internal pluralism of the media. However, this effect seems not be robust against designs such as the donut-difference-in-difference design. Standard theories of political information typically assume that information is generally beneficial for democratic accountability. This study, however, suggests that information in the period leading up to elections may be strongly slanted and may lack internal pluralism. This raises an important follow-up question: what effect does exposure to differently slanted media environments have on voters’ political attitudes?

At the same time, the results should be interpreted with caution. I use regions to construct treatment and control groups, but even with this strategy, the SUTVA assumption is likely violated. Historical newspaper outlets often had readerships that extended well beyond their immediate region, and major outlets were not consumed exclusively locally. An event-study design can partially assess such spillover dynamics. However, this issue is not necessarily fatal to the design, as such spillovers would most likely bias the estimated coefficients toward zero.

A more serious concern relates to the internal validity of the measurement strategy. The language model I use has relatively few parameters and therefore has limited ability to understand context. This may create substantial problems, especially for the classification of tone and particularly when comparing texts across long historical periods. In addition, I classify only article headlines rather than full article content. This is another important limitation, since headlines may be more sensationalist and less balanced than the underlying article text, which may provide a more nuanced or pluralistic discussion.

Another central limitation concerns the generalizability of the findings. The small number of newspaper outlets included in the analysis contributes to instability and limited robustness in the estimates. More importantly, the newspapers in my sample may not be representative of the broader historical media landscape. A large share of the articles in the dataset comes from the Chicago Tribune and the New York Times, which gives these outlets disproportionate influence over the results.

Despite these limitations, I see the findings of this study as promising for future research, particularly work that extends this perspective to other media such as radio, television, or social media. I believe that this type of media research is of substantial societal and political relevance, because democratic accountability depends on the availability of information, and less balanced media environments may contribute to democratic erosion and political polarization.

# References
::: {#refs}
:::

\newpage

# Appendix

## A. Measurement Specifications

### TDM Studio Search

In TDM studio I restricted the artcles to english news and articles in which the full content mentions 
"congress, senate,  election, campaign,  representative, senator".  Specifically, I am using the following prompt:
*LA(english) AND FULLTEXT(congress OR senate OR election OR campaign OR representative OR senator)*

### LLM Prompt for slant

I used the following prompt:

"
You are a political text annotation assistant.

Task:
Classify the partisan framing of this U.S. newspaper headline.

Return ONE of:
- D  = headline favors Democrats / the liberal side, or criticizes Republicans / the conservative side
- R  = headline favors Republicans / the conservative side, or criticizes Democrats / the liberal side
- N  = neutral or balanced — no clear partisan slant
- NA = not a U.S. political headline, or insufficient context to determine partisan framing

Definitions:
- D: The wording explicitly portrays Democrats, liberals, or the left more favorably, or portrays Republicans, conservatives, or the right more negatively (e.g., loaded praise for Democrats, blame language toward Republicans).
- R: The wording explicitly portrays Republicans, conservatives, or the right more favorably, or portrays Democrats, liberals, or the left more negatively.
- N: Neutral, factual, or symmetric reporting. No evaluative slant toward either side. Includes straightforward reporting of political events, votes, or statements without loaded language.
- NA: The headline is not about U.S. politics, involves no partisan actors, or is too vague or incomplete to classify.

Rules:
1. Use only the headline text. Do not use outside knowledge.
2. Evaluate explicit wording only — do not infer unstated meanings or assume bias from context.
3. Coverage of only one party is NOT automatically partisan. Return D or R only if the wording itself is evaluative or loaded. If coverage is neutral and factual, return N even if only one party is mentioned.
4. If uncertain between D and R, return N.
5. Return only one value: D, R, N, or NA.

Headline: {headline}
"

## B. Robustness: Illness (fast) Subsample

Figure B1 replicates the main slant event study restricting the sample to special elections triggered by fast-onset illnesses only. This is the most conservative exogeneity restriction: these deaths were, by definition, not anticipated by the media environment. The pre-period is flat and the post-period pattern mirrors the main result, providing strong support for the causal interpretation.

![Event study for partisan slant: illness (fast) deaths only. Error bars show 95% CI clustered by event.](figures/appendix/app_fig_robust_ilfast.png){width=80%}

## B. HonestDiD Sensitivity Analysis

Figure B2 presents the @roth2023 sensitivity analysis for each of the four main DiD specifications: pooled, post-1970, exogenous deaths, and illness (slow). The horizontal axis shows $\bar{M}$, the maximum pre-trend violation as a multiple of the largest observed pre-period coefficient. The shaded bands display the robust confidence set under each violation level. A specification is considered robust if the confidence set excludes zero for small values of $\bar{M}$.

![HonestDiD sensitivity analysis. Each panel corresponds to one DiD specification. Shaded areas show robust confidence sets as $\bar{M}$ increases from 0 (standard CI) to 2.](figures/appendix/app_fig_honest_did.png){width=90%}

## C. Internal Pluralism: SD of Headline Slant

Figure  C3 shows the event-study estimates for internal pluralism, operationalized as the standard deviation of article-level headline slant within an outlet-week. A higher within-outlet standard deviation indicates more heterogeneous coverage. The figure is presented for two outlets with sufficient article density across the observation period (NYT, Chicago Tribune). Pre-trend tests fail, so these results should be interpreted as descriptive evidence only.

![Event study: internal media pluralism (SD of headline slant). Pre-trends fail; results are descriptive. NYT and Chicago Tribune only.](figures/analysis/12_fig_pluralism_paper.png){width=90%}
