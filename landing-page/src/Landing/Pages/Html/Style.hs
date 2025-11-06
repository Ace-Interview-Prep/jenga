
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Landing.Pages.Html.Style where

import qualified Data.Text as T
import Text.IStr (istr)

-- =========
-- Common CSS (as reusable fragments)
-- =========

newtype CSS = CSS String

css :: String -> CSS
css = CSS

commonContainer :: CSS
commonContainer = css [istr|
.container {
  width: 100%;
  margin-right: auto;
  margin-left: auto;
  padding-right: 1rem /* 16px */;
  padding-left: 1rem /* 16px */;
}
@media (min-width: 1400px) {
  .container { max-width: 1400px; }
}
|]

keyframesCore :: CSS
keyframesCore = css [istr|
/* Core keyframes */
@keyframes fadeInUp {
  from { opacity: 0; transform: translateY(30px); }
  to   { opacity: 1; transform: translateY(0); }
}
@keyframes expandWidth {
  from { transform: translateX(-50%) scaleX(0); }
  to   { transform: translateX(-50%) scaleX(1); }
}
@keyframes slideFade {
  from { opacity: 0; transform: translateX(20px); }
  to   { opacity: 1; transform: translateX(0); }
}
@keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }
@keyframes fadeOut { from { opacity: 1; } to { opacity: 0; } }
|]

keyframesSlideLR :: CSS
keyframesSlideLR = css [istr|
@keyframes slideInRight { from { transform: translateX(100%); } to { transform: translateX(0); } }
@keyframes slideOutRight { from { transform: translateX(0); } to { transform: translateX(100%); } }
@keyframes slideInLeft  { to { opacity: 1; transform: translateX(0); } }
|]

keyframesExtrasCommon :: CSS
keyframesExtrasCommon = css [istr|
@keyframes float { 0%,100% { transform: translateY(0px); } 50% { transform: translateY(-10px); } }
@keyframes pulse { 0%,100% { opacity: 1; } 50% { opacity: 0.8; } }
@keyframes priceChange { 0% { transform: scale(1); opacity: 1; } 50% { transform: scale(1.05); opacity: 0.7; } 100% { transform: scale(1); opacity: 1; } }
@keyframes shimmer { 0% { left: -100%; } 100% { left: 100%; } }
|]

animationUtilities :: CSS
animationUtilities = css [istr|
.animate-fade-in-up    { animation: fadeInUp 0.8s ease-out; }
.animate-expand-width  { animation: expandWidth 0.45s ease-out 0.25s both; }
.animate-slide-fade    { animation: slideFade 0.6s ease-out; }
.animate-price-change  { animation: priceChange 0.5s ease-out; }
.animate-float         { animation: float 3s ease-in-out infinite; }
.animate-pulse-slow    { animation: pulse 2s ease-in-out infinite; }
|]

menuTransitions :: CSS
menuTransitions = css [istr|
.menu-item-animate {
  opacity: 0;
  transform: translateY(20px);
  animation: fadeInUp 0.4s ease-out forwards;
}
.menu-item-delay-1 { animation-delay: 0.1s; }
.menu-item-delay-2 { animation-delay: 0.2s; }
.menu-item-delay-3 { animation-delay: 0.3s; }
.menu-item-delay-4 { animation-delay: 0.4s; }
.mobile-menu-enter { animation: slideInRight 0.3s ease-out; }
.mobile-menu-exit  { animation: slideOutRight 0.3s ease-out; }
.menu-backdrop     { animation: fadeIn 0.3s ease-out; }
.menu-backdrop-exit{ animation: fadeOut 0.3s ease-out; }
|]

utilitiesCommon :: CSS
utilitiesCommon = css [istr|
.shadow-nav { box-shadow: 0 4px 20px rgba(0,0,0,0.08); }
.shadow-soft-outline { box-shadow: 0 0 0 1px rgba(0,0,0,0.05), 0 1px 3px rgba(0,0,0,0.1); }
.scrollbar-hide { -ms-overflow-style: none; scrollbar-width: none; }
.scrollbar-hide::-webkit-scrollbar { display: none; }
.line-clamp-2 { display: -webkit-box; -webkit-line-clamp: 2; -webkit-box-orient: vertical; overflow: hidden; }
.scale-on-hover { transition: transform 0.3s ease; }
.scale-on-hover:hover { transform: scale(1.05); }
|]

fadeInSectionCommon :: CSS
fadeInSectionCommon = css [istr|
/* Intersection Observer animations */
.fade-in-section {
  opacity: 0;
  transform: translateY(30px);
  transition: opacity 0.8s ease-out, transform 0.8s ease-out;
}
.fade-in-section.is-visible { opacity: 1; transform: translateY(0); }
|]

primaryColorCommon :: CSS
primaryColorCommon = css [istr|
/* Custom primary color */
.text-primary { color: #14a9db; }
.bg-primary   { background-color: #14a9db; }
.bg-primary\/10 { background-color: rgba(20,169,219,0.1); }
|]

cardsCommon :: CSS
cardsCommon = css [istr|
/* Card hovers */
.pricing-card { transition: all 0.3s ease; }
.pricing-card:hover { transform: translateY(-5px); box-shadow: 0 20px 40px rgba(0,0,0,0.05); }
.cta-card { transition: all 0.3s ease; }
.cta-card:hover { transform: translateY(-2px); box-shadow: 0 10px 25px rgba(0,0,0,0.01); }
|]

customSelectCommon :: CSS
customSelectCommon = css [istr|
/* Custom select styling */
.custom-select {
  background-image: url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' d='M6 8l4 4 4-4'/%3e%3c/svg%3e");
  background-position: right 0.5rem center;
  background-repeat: no-repeat;
  background-size: 1.5em 1.5em;
  padding-right: 2.5rem;
}
|]

popularBadgeCommon :: CSS
popularBadgeCommon = css [istr|
.popular-badge { animation: pulse 2s infinite; }
|]

regionLoadingCommon :: CSS
regionLoadingCommon = css [istr|
.region-loading { position: relative; overflow: hidden; }
.region-loading::after {
  content: "";
  position: absolute;
  top: 0; left: -100%; width: 100%; height: 100%;
  background: linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent);
  animation: shimmer 1.5s infinite;
}
|]

priceTransitionCommon :: CSS
priceTransitionCommon = css [istr|
.price-container { position: relative; overflow: hidden; }
.price-old { position: absolute; top: 0; left: 0; opacity: 0; transform: translateY(-20px); transition: all 0.3s ease; }
.price-new { opacity: 1; transform: translateY(0); transition: all 0.3s ease; }
|]

-- =========
-- PAGE-SPECIFIC STYLE BUNDLES (names preserved)
-- =========

concatCSS :: [CSS] -> CSS
concatCSS = CSS . concatMap (\(CSS s) -> s)

aboutCustomStyleCss :: CSS
aboutCustomStyleCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , css [istr|
@keyframes marquee { 0% { transform: translateX(0%); } 100% { transform: translateX(-50%); } }
.animate-marquee { animation: marquee linear infinite; }
|]
  , animationUtilities
  , menuTransitions
  , utilitiesCommon
  , fadeInSectionCommon
  , primaryColorCommon
  , css [istr|
/* Marquee mask */
.marquee-container {
  mask-image: linear-gradient(to right, transparent, white 20%, white 80%, transparent);
  -webkit-mask-image: linear-gradient(to right, transparent, white 20%, white 80%, transparent);
}
/* Team card hover */
.team-card { transition: all 0.3s ease; }
.team-card:hover { transform: translateY(-5px); box-shadow: 0 10px 25px rgba(0,0,0,0.1); }
/* Video play hover */
.video-play-btn { transition: all 0.3s ease; }
.video-play-btn:hover { transform: scale(1.05); background-color: rgba(255,255,255,0.3); }
.stat-number { font-variant-numeric: tabular-nums; }
|]
  ]

engineersCustomStyleCss :: CSS
engineersCustomStyleCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , keyframesExtrasCommon
  , animationUtilities
  , menuTransitions
  , utilitiesCommon
  , fadeInSectionCommon
  , primaryColorCommon
  , css [istr|
/* Skills tab */
.skill-tab { position: relative; transition: all 0.3s ease; }
.skill-tab.active { color: #1f2937; font-weight: 500; }
.skill-tab.active::after { content:""; position:absolute; bottom:-2px; left:0; right:0; height:2px; background-color:#14a9db; border-radius:1px; }
/* Hero image */
.hero-image { transition: transform 0.3s ease; }
.hero-image:hover { transform: scale(1.02); }
/* Feature list */
.feature-item { opacity:0; transform:translateX(-20px); animation: slideInLeft 0.6s ease-out forwards; }
.feature-item:nth-child(1){ animation-delay:0.1s; }
.feature-item:nth-child(2){ animation-delay:0.2s; }
.feature-item:nth-child(3){ animation-delay:0.3s; }
/* Button shimmer */
.btn-primary { position:relative; overflow:hidden; transition:all 0.3s ease; }
.btn-primary::before { content:""; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg,transparent,rgba(255,255,255,0.2),transparent); transition:left 0.5s; }
.btn-primary:hover::before { left:100%; }
/* Globe */
.globe-container { position: relative; overflow: hidden; }
.globe-image { transition: transform 0.3s ease; }
.globe-container:hover .globe-image { transform: scale(1.05); }
/* Scholarship section */
.scholarship-section { background: linear-gradient(135deg, rgba(20,169,219,0.05) 0%, rgba(42,137,220,0.05) 100%); }
/* Responsive tweaks */
@media (max-width: 1024px) { .hero-section { padding-top: 6rem; } }
@media (max-width: 768px) { .hero-section { padding-top: 4rem; } }
|]
  ]

pricingCustomStyleCss :: CSS
pricingCustomStyleCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , css [istr|
@keyframes priceChange { 0%{transform:scale(1);opacity:1;} 50%{transform:scale(1.05);opacity:.7;} 100%{transform:scale(1);opacity:1;} }
|]
  , animationUtilities
  , menuTransitions
  , utilitiesCommon
  , fadeInSectionCommon
  , primaryColorCommon
  , cardsCommon
  , customSelectCommon
  , popularBadgeCommon
  , regionLoadingCommon
  , priceTransitionCommon
  ]

blogStyleTagCss :: CSS
blogStyleTagCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , keyframesExtrasCommon
  , animationUtilities
  , menuTransitions
  , utilitiesCommon
  , fadeInSectionCommon
  , primaryColorCommon
  , css [istr|
.line-clamp-6 { display:-webkit-box; -webkit-line-clamp:6; -webkit-box-orient: vertical; overflow: hidden; }
.blog-card { transition: all 0.3s ease; }
.blog-card:hover { transform: translateY(-5px); box-shadow: 0 20px 40px rgba(0,0,0,0.1); }
.blog-card-image { transition: transform 0.3s ease; }
.blog-card:hover .blog-card-image { transform: scale(1.05); }
.btn-primary { position:relative; overflow:hidden; transition:all 0.3s ease; }
.btn-primary::before { content:""; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg,transparent,rgba(255,255,255,0.2),transparent); transition:left 0.5s; }
.btn-primary:hover::before { left:100%; }
.blog-card-animate { opacity:0; transform:translateY(30px); animation: fadeInUp 0.6s ease-out forwards; }
.blog-card-animate:nth-child(1){ animation-delay:0.1s; }
.blog-card-animate:nth-child(2){ animation-delay:0.2s; }
.blog-card-animate:nth-child(3){ animation-delay:0.3s; }
.blog-card-animate:nth-child(4){ animation-delay:0.4s; }
.featured-blog-card { transition: all 0.3s ease; }
.featured-blog-card:hover { transform: translateY(-3px); box-shadow: 0 15px 30px rgba(0,0,0,0.08); }
.blog-tag { transition: all 0.2s ease; }
.blog-tag:hover { background-color:#14a9db; color:white; }
.blog-title { transition: color 0.3s ease; }
.blog-title:hover { color:#14a9db; }
.read-more-btn { transition: all 0.3s ease; }
.read-more-btn:hover { background-color:#f9fafb; border-color:#d1d5db; transform: translateX(2px); }
.read-more-btn:hover svg { transform: translateX(2px); }
@media (max-width: 768px) { .blog-grid { grid-template-columns: 1fr; } }
@media (min-width:769px) and (max-width:1024px) { .blog-grid { grid-template-columns: repeat(2,1fr); } }
@media (min-width:1025px) { .blog-grid { grid-template-columns: repeat(2,1fr); } }
.blog-meta { position: relative; }
.blog-meta::before { content:""; position:absolute; left:0; top:50%; width:24px; height:1px; background-color:#9ca3af; transform: translateY(-50%); }
.hero-section { background: linear-gradient(135deg, rgba(20,169,219,0.02) 0%, rgba(42,137,220,0.02) 100%); }
|]
  ]

faqCustomStyleCss :: CSS
faqCustomStyleCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , css [istr|
@keyframes expandHeight { from { max-height: 0; opacity: 0; } to { max-height: 500px; opacity: 1; } }
@keyframes collapseHeight { from { max-height: 500px; opacity: 1; } to { max-height: 0; opacity: 0; } }
|]
  , css [istr|
.animate-fade-in-up { animation: fadeInUp 0.6s ease-out forwards; }
.animate-slide-in-left { animation: slideInLeft 0.8s ease-out forwards; }
.animate-slide-in-right{ animation: slideInRight 0.8s ease-out forwards; }
|]
  , menuTransitions
  , utilitiesCommon
  , fadeInSectionCommon
  , primaryColorCommon
  , css [istr|
/* Accordion */
.accordion-content { max-height:0; overflow:hidden; transition:all 0.4s cubic-bezier(0.4,0,0.2,1); opacity:0; }
.accordion-item.open .accordion-content { max-height:500px; opacity:1; padding-top:1rem; padding-bottom:1.5rem; }
.accordion-icon { transition: transform 0.3s cubic-bezier(0.4,0,0.2,1); }
.accordion-item.open .accordion-icon { transform: rotate(180deg); }
.accordion-item { transition: all 0.3s ease; }
.accordion-item:hover { transform: translateY(-2px); box-shadow: 0 10px 25px rgba(0,0,0,0.08); }
.accordion-header { transition: all 0.3s ease; }
.accordion-header:hover { background-color: rgba(249,250,251,0.5); }
.accordion-item.open .accordion-header { background-color: rgba(20,169,219,0.02); }
/* Staggered FAQ items */
.faq-item-animate { opacity:0; transform:translateY(30px); animation: fadeInUp 0.6s ease-out forwards; }
.faq-item-animate:nth-child(1){ animation-delay:0.1s; }
.faq-item-animate:nth-child(2){ animation-delay:0.2s; }
.faq-item-animate:nth-child(3){ animation-delay:0.3s; }
.faq-item-animate:nth-child(4){ animation-delay:0.4s; }
.faq-item-animate:nth-child(5){ animation-delay:0.5s; }
.faq-item-animate:nth-child(6){ animation-delay:0.6s; }
.faq-item-animate:nth-child(7){ animation-delay:0.7s; }
/* Buttons + util */
.btn-primary { position:relative; overflow:hidden; transition:all 0.3s ease; }
.btn-primary::before { content:""; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg,transparent,rgba(255,255,255,0.2),transparent); transition:left 0.5s; }
.btn-primary:hover::before { left:100%; }
|]
  ]

contactHeadStyleCss :: CSS
contactHeadStyleCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , css [istr|
@keyframes shimmer { 0% { background-position: -200px 0; } 100% { background-position: calc(200px + 100%) 0; } }
@keyframes pulse { 0%,100% { opacity: 1; } 50% { opacity: 0.5; } }
@keyframes bounce { 0%,20%,53%,80%,100% { transform: translate3d(0,0,0); } 40%,43% { transform: translate3d(0,-30px,0); } 70% { transform: translate3d(0,-15px,0); } 90% { transform: translate3d(0,-4px,0); } }
@keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
|]
  , menuTransitions
  , utilitiesCommon
  , fadeInSectionCommon
  , primaryColorCommon
  , css [istr|
.animate-fade-in-up { animation: fadeInUp 0.8s ease-out; }
.animate-fade-in-up-delay { animation: fadeInUp 0.8s ease-out 0.2s both; }
.animate-fade-in-up-delay-2 { animation: fadeInUp 0.8s ease-out 0.4s both; }
.form-shimmer { background: linear-gradient(90deg,#f0f0f0 25%,#e0e0e0 50%,#f0f0f0 75%); background-size: 200px 100%; animation: shimmer 2s infinite; }
.form-field-focus { transform: translateY(-2px); box-shadow: 0 8px 25px rgba(20,169,219,0.15); }
.form-success { animation: bounce 0.6s ease-out; }
.form-error { animation: pulse 0.5s ease-out; }
.loading-spinner { border:2px solid #f3f3f3; border-top:2px solid #14a9db; border-radius:50%; width:20px; height:20px; animation: spin 1s linear infinite; }
.button-shimmer { position:relative; overflow:hidden; }
.button-shimmer::before { content:""; position:absolute; top:0; left:-100%; width:100%; height:100%; background:linear-gradient(90deg,transparent,rgba(255,255,255,0.2),transparent); transition:left 0.5s; }
.button-shimmer:hover::before { left:100%; }
.social-hover { transition: all 0.3s ease; }
.social-hover:hover { transform: translateY(-3px) scale(1.1); box-shadow: 0 8px 25px rgba(20,169,219,0.2); }
.stagger-animation { opacity:0; transform:translateY(20px); transition: opacity 0.6s ease-out, transform 0.6s ease-out; }
.stagger-animation.is-visible { opacity:1; transform:translateY(0); }
.stagger-delay-1 { transition-delay: 0.1s; }
.stagger-delay-2 { transition-delay: 0.2s; }
.stagger-delay-3 { transition-delay: 0.3s; }
.stagger-delay-4 { transition-delay: 0.4s; }
.stagger-delay-5 { transition-delay: 0.5s; }
.custom-focus:focus { outline:none; ring:2px; ring-color:#14a9db; border-color:#14a9db; }
.field-valid { border-color:#10b981; background-color:#f0fdf4; }
.field-invalid { border-color:#ef4444; background-color:#fef2f2; }
.error-message { color:#ef4444; font-size:0.75rem; margin-top:0.25rem; opacity:0; transform:translateY(-10px); transition: all 0.3s ease; }
.error-message.show { opacity:1; transform:translateY(0); }
|]
  ]

-- =========
-- OPTIONAL: a minimal generic block if/when you need a quick style
-- =========

customStyleCss :: CSS
customStyleCss = concatCSS
  [ commonContainer
  , keyframesCore
  , keyframesSlideLR
  , animationUtilities
  , utilitiesCommon
  , fadeInSectionCommon
  ]

----------- | All Below is Old, keeping around just in case----------|

-- | Custom CSS styles (embedded version of your large CSS block)
customStyles2 :: T.Text
customStyles2 = T.pack [istr|
.container {
  width: 100%;
  margin-right: auto;
  margin-left: auto;
  padding-right: 1rem;
  padding-left: 1rem;
}
@media (min-width: 1400px) {
  .container {
    max-width: 1400px;
  }
}
@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}
.animate-fade-in-up {
  animation: fadeInUp 0.8s ease-out;
}
.text-primary {
  color: #14a9db;
}
.bg-primary {
  background-color: #14a9db;
}
.shadow-nav {
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.025);
}
.pricing-card {
  transition: all 0.3s ease;
}
.pricing-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.05);
}
|]

-- | Custom CSS styles (full version from your original code)
customStyles :: T.Text
customStyles = T.pack [istr|
.container {
  width: 100%;
  margin-right: auto;
  margin-left: auto;
  padding-right: 1rem /* 16px */;
  padding-left: 1rem /* 16px */;
}
@media (min-width: 1400px) {
  .container {
    max-width: 1400px;
  }
}
/* Custom animations */
@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(30px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

@keyframes expandWidth {
  from {
    transform: translateX(-50%) scaleX(0);
  }
  to {
    transform: translateX(-50%) scaleX(1);
  }
}

@keyframes slideFade {
  from {
    opacity: 0;
    transform: translateX(20px);
  }
  to {
    opacity: 1;
    transform: translateX(0);
  }
}

@keyframes priceChange {
  0% {
    transform: scale(1);
    opacity: 1;
  }
  50% {
    transform: scale(1.05);
    opacity: 0.7;
  }
  100% {
    transform: scale(1);
    opacity: 1;
  }
}

.animate-fade-in-up {
  animation: fadeInUp 0.8s ease-out;
}

.animate-expand-width {
  animation: expandWidth 0.45s ease-out 0.25s both;
}

.animate-slide-fade {
  animation: slideFade 0.6s ease-out;
}

.animate-price-change {
  animation: priceChange 0.5s ease-out;
}

.menu-item-animate {
  opacity: 0;
  transform: translateY(20px);
  animation: fadeInUp 0.4s ease-out forwards;
}

.menu-item-delay-1 {
  animation-delay: 0.1s;
}
.menu-item-delay-2 {
  animation-delay: 0.2s;
}
.menu-item-delay-3 {
  animation-delay: 0.3s;
}
.menu-item-delay-4 {
  animation-delay: 0.4s;
}

.mobile-menu-enter {
  animation: slideInRight 0.3s ease-out;
}

.mobile-menu-exit {
  animation: slideOutRight 0.3s ease-out;
}

.menu-backdrop {
  animation: fadeIn 0.3s ease-out;
}

.menu-backdrop-exit {
  animation: fadeOut 0.3s ease-out;
}

@keyframes slideInRight {
  from {
    transform: translateX(100%);
  }
  to {
    transform: translateX(0);
  }
}

@keyframes slideOutRight {
  from {
    transform: translateX(0);
  }
  to {
    transform: translateX(100%);
  }
}

@keyframes fadeIn {
  from {
    opacity: 0;
  }
  to {
    opacity: 1;
  }
}

@keyframes fadeOut {
  from {
    opacity: 1;
  }
  to {
    opacity: 0;
  }
}

.shadow-nav {
  box-shadow: 0 4px 20px rgba(0, 0, 0, 0.025);
}

.shadow-soft-outline {
  box-shadow: 0 0 0 1px rgba(0, 0, 0, 0.05), 0 1px 3px rgba(0, 0, 0, 0.1);
}

.scrollbar-hide {
  -ms-overflow-style: none;
  scrollbar-width: none;
}

.scrollbar-hide::-webkit-scrollbar {
  display: none;
}

.line-clamp-2 {
  display: -webkit-box;
  -webkit-line-clamp: 2;
  -webkit-box-orient: vertical;
  overflow: hidden;
}

/* Intersection Observer animations */
.fade-in-section {
  opacity: 0;
  transform: translateY(30px);
  transition: opacity 0.8s ease-out, transform 0.8s ease-out;
}

.fade-in-section.is-visible {
  opacity: 1;
  transform: translateY(0);
}

.scale-on-hover {
  transition: transform 0.3s ease;
}

.scale-on-hover:hover {
  transform: scale(1.05);
}

/* Custom primary color */
.text-primary {
  color: #14a9db;
}

.bg-primary {
  background-color: #14a9db;
}

.bg-primary\/10 {
  background-color: rgba(20, 169, 219, 0.1);
}

/* Pricing card hover effects */
.pricing-card {
  transition: all 0.3s ease;
}

.pricing-card:hover {
  transform: translateY(-5px);
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.05);
}

/* CTA card hover effects */
.cta-card {
  transition: all 0.3s ease;
}

.cta-card:hover {
  transform: translateY(-2px);
  box-shadow: 0 10px 25px rgba(0, 0, 0, 0.01);
}

/* Custom select styling */
.custom-select {
  background-image: url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' fill='none' viewBox='0 0 20 20'%3e%3cpath stroke='%236b7280' stroke-linecap='round' stroke-linejoin='round' stroke-width='1.5' d='M6 8l4 4 4-4'/%3e%3c/svg%3e");
  background-position: right 0.5rem center;
  background-repeat: no-repeat;
  background-size: 1.5em 1.5em;
  padding-right: 2.5rem;
}

/* Popular badge animation */
.popular-badge {
  animation: pulse 2s infinite;
}

@keyframes pulse {
  0%,
  100% {
    opacity: 1;
  }
  50% {
    opacity: 0.8;
  }
}

/* Region selector loading animation */
.region-loading {
  position: relative;
  overflow: hidden;
}

.region-loading::after {
  content: "";
  position: absolute;
  top: 0;
  left: -100%;
  width: 100%;
  height: 100%;
  background: linear-gradient(
    90deg,
    transparent,
    rgba(255, 255, 255, 0.4),
    transparent
  );
  animation: shimmer 1.5s infinite;
}

@keyframes shimmer {
  0% {
    left: -100%;
  }
  100% {
    left: 100%;
  }
}

/* Price transition effects */
.price-container {
  position: relative;
  overflow: hidden;
}

.price-old {
  position: absolute;
  top: 0;
  left: 0;
  opacity: 0;
  transform: translateY(-20px);
  transition: all 0.3s ease;
}

.price-new {
  opacity: 1;
  transform: translateY(0);
  transition: all 0.3s ease;
}
|]
