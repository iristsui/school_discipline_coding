\name{a}
\alias{a}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(a)}
\format{
  A data frame with 231 observations on the following 60 variables.
  \describe{
    \item{\code{school_stateid}}{a numeric vector}
    \item{\code{school_schoolyear}}{an ordered factor with levels \code{200405} < \code{200506} < \code{200607} < \code{200708}}
    \item{\code{school_educationalorgid}}{an ordered factor with levels \code{217} < \code{218} < \code{220} < \code{327} < \code{328} < \code{357} < \code{361} < \code{368} < \code{568} < \code{844} < \code{885} < \code{886} < \code{922} < \code{930} < \code{943} < \code{955} < \code{978} < \code{980} < \code{982} < \code{1094} < \code{1117} < \code{1594} < \code{1766} < \code{1936} < \code{1939} < \code{1940} < \code{1941} < \code{1997} < \code{2203} < \code{2233} < \code{2234} < \code{2295} < \code{2296} < \code{2305} < \code{2576} < \code{2889} < \code{2897} < \code{3006} < \code{3037} < \code{3056} < \code{3057} < \code{3075} < \code{3079} < \code{3080} < \code{3084} < \code{3085} < \code{3086} < \code{3087} < \code{3115} < \code{3192} < \code{3212} < \code{3259} < \code{3260} < \code{3295} < \code{3296} < \code{3302} < \code{3303} < \code{3306} < \code{3451} < \code{3474} < \code{3607} < \code{3700} < \code{3810} < \code{4044}}
    \item{\code{school_ncesschoolid}}{an ordered factor with levels \code{80001601821} < \code{80540001786} < \code{80594001017} < \code{80612001039} < \code{80612001043} < \code{80612001051} < \code{80612001053} < \code{80612001055} < \code{80612001061} < \code{80612001067} < \code{80723001234} < \code{80723001240} < \code{80723001244} < \code{170468000040} < \code{170468000144} < \code{170468000149} < \code{170651000328} < \code{170855000477} < \code{170855000480} < \code{170855000486} < \code{171101001332} < \code{171200001447} < \code{171455001805} < \code{172734002898} < \code{172761002925} < \code{172761002926} < \code{173042003157} < \code{173441003529} < \code{173441003538} < \code{173441003540} < \code{174065004087} < \code{174169004170} < \code{174169004354} < \code{174169005293} < \code{174386004351} < \code{240006000036} < \code{240006000040} < \code{240006000041} < \code{240006000050} < \code{240006000059} < \code{240006000072} < \code{240006000075} < \code{240006000087} < \code{240006000090} < \code{240006000091} < \code{240006000095} < \code{240006000099} < \code{240006000107} < \code{240006000108} < \code{240006001403} < \code{240006090441} < \code{240015000027} < \code{240015000507} < \code{240015000967} < \code{240015002946} < \code{240027000599} < \code{301326000389} < \code{410162000153} < \code{410204000621} < \code{410204000622} < \code{410474000561} < \code{410474000562} < \code{410474000564} < \code{411052001397}}
    \item{\code{school_districtid}}{a numeric vector}
    \item{\code{school_lowgradeid}}{a factor with levels \code{1} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{11} \code{14} \code{15}}
    \item{\code{school_lowgradelabel}}{a factor with levels \code{11} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{K} \code{Pre K} \code{1} \code{3}}
    \item{\code{school_highgradeid}}{a factor with levels \code{5} \code{6} \code{7} \code{8} \code{10} \code{12}}
    \item{\code{school_highgradelabel}}{a factor with levels \code{11} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9} \code{K} \code{Pre K} \code{1} \code{3}}
    \item{\code{nces_femaleenrollment}}{a numeric vector}
    \item{\code{nces_maleenrollment}}{a numeric vector}
    \item{\code{school_schoolyear_nces}}{a factor with levels \code{200405} \code{200506} \code{200607} \code{200708}}
    \item{\code{nces_ncessch}}{a factor with levels \code{80001601821} \code{80001701817} \code{80001701818} \code{80195000018} \code{80291000197} \code{80480000706} \code{80480000714} \code{80480000788} \code{80480000801} \code{80480001448} \code{80540001786} \code{80594001017} \code{80612001039} \code{80612001040} \code{80612001043} \code{80612001051} \code{80612001053} \code{80612001055} \code{80612001057} \code{80612001059} \code{80612001061} \code{80612001067} \code{80612001995} \code{80675001159} \code{80723001234} \code{80723001240} \code{80723001244} \code{170468000040} \code{170468000144} \code{170468000149} \code{170651000328} \code{170651004244} \code{170855000477} \code{170855000480} \code{170855000486} \code{170855002897} \code{170855005298} \code{170993000771} \code{170993000782} \code{170993000787} \code{170993000790} \code{170993001045} \code{170993001107} \code{170993005410} \code{170993005485} \code{171101001332} \code{171200001447} \code{171314000063} \code{171314003421} \code{171446001794} \code{171455001805} \code{171455001806} \code{172163002426} \code{172640002813} \code{172734002898} \code{172761002925} \code{172761002926} \code{172862002996} \code{172862005695} \code{173042003157} \code{173441003526} \code{173441003527} \code{173441003529} \code{173441003530} \code{173441003534} \code{173441003538} \code{173441003540} \code{173441003544} \code{173441005589} \code{173951003996} \code{174007004027} \code{174007004030} \code{174065004087} \code{174169004170} \code{174169004354} \code{174169005293} \code{174386004351} \code{174386004353} \code{240006000036} \code{240006000040} \code{240006000041} \code{240006000050} \code{240006000058} \code{240006000059} \code{240006000072} \code{240006000075} \code{240006000086} \code{240006000087} \code{240006000090} \code{240006000091} \code{240006000095} \code{240006000099} \code{240006000107} \code{240006000108} \code{240006001403} \code{240006090441} \code{240015000027} \code{240015000507} \code{240015000967} \code{240015002946} \code{240027000599} \code{240051000865} \code{240072001333} \code{260366004061} \code{261995005656} \code{262578006232} \code{301326000389} \code{330213000033} \code{330660000432} \code{330705000004} \code{380546000155} \code{410004000176} \code{410162000153} \code{410192001204} \code{410204000621} \code{410204000622} \code{410204001391} \code{410204004179} \code{410326000173} \code{410474000531} \code{410474000542} \code{410474000550} \code{410474000561} \code{410474000562} \code{410474000564} \code{410474000565} \code{410933000112} \code{410933000113} \code{410933000114} \code{410933000116} \code{410933000118} \code{410933000121} \code{410933000147} \code{411052001397} \code{411308000166}}
    \item{\code{nces_type}}{a factor with levels \code{1} \code{2} \code{4}}
    \item{\code{nces_locale}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{6} \code{7} \code{8}}
    \item{\code{nces_ulocal}}{a factor with levels \code{11} \code{12} \code{13} \code{21} \code{22} \code{23} \code{32} \code{33} \code{41} \code{42} \code{43}}
    \item{\code{nces_fte}}{a numeric vector}
    \item{\code{nces_gslo}}{an ordered factor with levels \code{06} < \code{07} < \code{08} < \code{09}}
    \item{\code{nces_gshi}}{an ordered factor with levels \code{8} < \code{10} < \code{12}}
    \item{\code{nces_level}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{nces_totfrl}}{a numeric vector}
    \item{\code{nces_g06}}{a numeric vector}
    \item{\code{nces_g07}}{a numeric vector}
    \item{\code{nces_g08}}{a numeric vector}
    \item{\code{nces_g09}}{a numeric vector}
    \item{\code{nces_g10}}{a numeric vector}
    \item{\code{nces_g11}}{a numeric vector}
    \item{\code{nces_g12}}{a numeric vector}
    \item{\code{nces_member}}{a numeric vector}
    \item{\code{nces_am}}{a numeric vector}
    \item{\code{nces_amalm}}{a numeric vector}
    \item{\code{nces_amalf}}{a numeric vector}
    \item{\code{nces_asian}}{a numeric vector}
    \item{\code{nces_asalm}}{a numeric vector}
    \item{\code{nces_asalf}}{a numeric vector}
    \item{\code{nces_hisp}}{a numeric vector}
    \item{\code{nces_hialm}}{a numeric vector}
    \item{\code{nces_hialf}}{a numeric vector}
    \item{\code{nces_black}}{a numeric vector}
    \item{\code{nces_blalm}}{a numeric vector}
    \item{\code{nces_blalf}}{a numeric vector}
    \item{\code{nces_white}}{a numeric vector}
    \item{\code{nces_whalm}}{a numeric vector}
    \item{\code{nces_whalf}}{a numeric vector}
    \item{\code{grade.type}}{an ordered factor with levels \code{MS} < \code{HS} < \code{Mix}}
    \item{\code{high.school}}{a numeric vector}
    \item{\code{school.statelabel}}{a factor with levels \code{CO} \code{IL} \code{MD} \code{MT} \code{OR}}
    \item{\code{school.instruc.days}}{a factor with levels \code{150} \code{176} \code{180}}
    \item{\code{enrollment.cat}}{an ordered factor with levels \code{S} < \code{M} < \code{L}}
    \item{\code{enrollment.sq}}{a numeric vector}
    \item{\code{female.prop}}{a numeric vector}
    \item{\code{male.prop}}{a numeric vector}
    \item{\code{afram.prop}}{a numeric vector}
    \item{\code{aian.prop}}{a numeric vector}
    \item{\code{asian.prop}}{a numeric vector}
    \item{\code{latino.prop}}{a numeric vector}
    \item{\code{white.prop}}{a numeric vector}
    \item{\code{fte.100.student}}{a numeric vector}
    \item{\code{frpm.prop}}{a numeric vector}
    \item{\code{year}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(a)
## maybe str(a) ; plot(a) ...
}
\keyword{datasets}
