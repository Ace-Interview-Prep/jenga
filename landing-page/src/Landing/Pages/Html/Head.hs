module Landing.Pages.Html.Head where

import Landing.Pages.Html.Style
import Landing.Static (static)
import Reflex.Dom.Core
import Control.Monad
import qualified Data.Text as T
import Data.Default
import Text.IStr

-- | Build complete HTML head with configuration
buildHtmlHead :: DomBuilder t m => HeadConfig -> m ()
buildHtmlHead config = do
  buildBasicMeta config
  buildSeoMeta config
  buildOpenGraphMeta config
  buildTwitterMeta config
  buildLinkTags config
  buildStructuredData config
  buildStylesAndScripts config

-- | Pre-configured functions for specific pages
aboutPageHead :: DomBuilder t m => m ()
aboutPageHead = buildHtmlHead $ def
  { headTitle = "About Us - Ace Talent"
  , headCanonicalUrl = "https://acetalent.com/about"
  , headOgUrl = "https://acetalent.com/about"
  , headStructuredDataUrl = "https://acetalent.com/about"
  , headStyles = [aboutCustomStyleCss]
  , headIncludeFontAwesome = True
  , headIncludeSwiper = True
  }

engineersPageHead :: DomBuilder t m => m ()
engineersPageHead = buildHtmlHead $ def
  { headTitle = "For Engineers - Ace Talent"
  , headCanonicalUrl = "https://acetalent.com/engineers"
  , headOgUrl = "https://acetalent.com/engineers"
  , headStructuredDataUrl = "https://acetalent.com/engineers"
  , headIncludeSwiper = True
  , headIncludeFontAwesome = True
  }

pricingPageHead :: DomBuilder t m => m ()
pricingPageHead = buildHtmlHead $ def
  { headTitle = "Pricing - Ace Talent"
  , headCanonicalUrl = "https://acetalent.com/pricing"
  , headOgUrl = "https://acetalent.com/pricing"
  , headStructuredDataUrl = "https://acetalent.com/pricing"
  }

blogPageHead :: DomBuilder t m => m ()
blogPageHead = buildHtmlHead $ def
  { headTitle = "Blog - Ace Talent"
  , headCanonicalUrl = "https://acetalent.com/blog"
  , headOgUrl = "https://acetalent.com/blog"
  , headStructuredDataUrl = "https://acetalent.com/blog"
  , headIncludeSwiper = True
  , headIncludeFontAwesome = True
  }

faqPageHead :: DomBuilder t m => m ()
faqPageHead = buildHtmlHead $ def
  { headTitle = "FAQ - Ace Talent | Frequently Asked Questions"
  , headDescription = "Find answers to frequently asked questions about Ace Talent's services, talent placement, vetting process, and more. Get the information you need to make informed decisions."
  , headKeywords = "FAQ, frequently asked questions, talent placement, developer hiring, technical vetting, Ace Talent support"
  , headCanonicalUrl = "https://acetalent.com/faq"
  , headOgTitle = "FAQ - Ace Talent | Frequently Asked Questions"
  , headOgDescription = "Find answers to frequently asked questions about Ace Talent's services and talent placement process."
  , headOgImage = "/og-faq.jpg"
  , headOgUrl = "https://acetalent.com/faq"
  , headTwitterTitle = "FAQ - Ace Talent | Frequently Asked Questions"
  , headTwitterDescription = "Find answers to frequently asked questions about Ace Talent's services."
  , headTwitterImage = "/twitter-faq.jpg"
  , headStructuredDataName = "Ace Talent FAQ"
  , headStructuredDataDescription = "Frequently asked questions about Ace Talent's services and talent placement process."
  , headStructuredDataUrl = "https://acetalent.com/faq"
  , headIncludeFontAwesome = True
  }

contactPageHead :: DomBuilder t m => m ()
contactPageHead = buildHtmlHead $ def
  { headTitle = "Contact Us - Ace Talent"
  , headCanonicalUrl = "https://acetalent.com/contact"
  , headOgUrl = "https://acetalent.com/contact"
  , headStructuredDataUrl = "https://acetalent.com/contact"
  , headIncludeFontAwesome = True
  }


-- | Index/Homepage head configuration
indexPageHead :: DomBuilder t m => m ()
indexPageHead = buildHtmlHead $ def
  { headTitle = "Ace Talent - We Build Great Engineers"
  , headDescription = "Create your Ace Talent account and join thousands of elite developers. Access exclusive opportunities, resources, and connect with top-tier companies hiring remote talent."
  , headKeywords = "developer signup, remote jobs, software engineer, tech talent, programming jobs, developer community, coding careers"
  , headCanonicalUrl = "https://acetalent.com"
  , headOgTitle = "Sign Up - Join Ace Talent | Premium Developer Community"
  , headOgDescription = "Create your Ace Talent account and join thousands of elite developers. Access exclusive opportunities and resources."
  , headOgImage = "/og-signup.jpg"
  , headOgUrl = "https://acetalent.com"
  , headTwitterTitle = "Sign Up - Join Ace Talent | Premium Developer Community"
  , headTwitterDescription = "Create your Ace Talent account and join thousands of elite developers."
  , headTwitterImage = "/twitter-signup.jpg"
  , headStructuredDataName = "Sign Up - Ace Talent"
  , headStructuredDataDescription = "Create your Ace Talent account and join thousands of elite developers."
  , headStructuredDataUrl = "https://acetalent.com/signup"
  , headIncludeSwiper = True
  , headIncludeFontAwesome = True
  }

-- | Configuration for HTML head elements
data HeadConfig = HeadConfig
  { headTitle :: T.Text
  , headDescription :: T.Text
  , headKeywords :: T.Text
  , headCanonicalUrl :: T.Text
  , headOgTitle :: T.Text
  , headOgDescription :: T.Text
  , headOgImage :: T.Text
  , headOgUrl :: T.Text
  , headTwitterTitle :: T.Text
  , headTwitterDescription :: T.Text
  , headTwitterImage :: T.Text
  , headStructuredDataName :: T.Text
  , headStructuredDataDescription :: T.Text
  , headStructuredDataUrl :: T.Text
  , headIncludeSwiper :: Bool
  , headIncludeFontAwesome :: Bool
  , headStyles :: [CSS]
  }

-- | Default configuration
instance Default HeadConfig where
  def = HeadConfig
    { headTitle = "Ace Talent - WE Build Great Engineers"
    , headDescription = "Create your Ace Talent account and join thousands of elite developers. Access exclusive opportunities, resources, and connect with top-tier companies hiring remote talent."
    , headKeywords = "developer signup, remote jobs, software engineer, tech talent, programming jobs, developer community, coding careers"
    , headCanonicalUrl = "https://acetalent.com"
    , headOgTitle = "Ace Talent - Premium Developer Community"
    , headOgDescription = "Create your Ace Talent account and join thousands of elite developers. Access exclusive opportunities and resources."
    , headOgImage = "/og-default.jpg"
    , headOgUrl = "https://acetalent.com"
    , headTwitterTitle = "Ace Talent - Premium Developer Community"
    , headTwitterDescription = "Create your Ace Talent account and join thousands of elite developers."
    , headTwitterImage = "/twitter-default.jpg"
    , headStructuredDataName = "Ace Talent"
    , headStructuredDataDescription = "Create your Ace Talent account and join thousands of elite developers."
    , headStructuredDataUrl = "https://acetalent.com"
    , headIncludeSwiper = False -- prefer optimized
    , headIncludeFontAwesome = False --prefer optimized
    , headStyles = [CSS $ T.unpack customStyles] -- If nothing set, then use the full thing -> Unoptimized
    }

-- | Constants used across all pages
siteName :: T.Text
siteName = "Ace Talent"

twitterHandle :: T.Text
twitterHandle = "@acetalent"

organizationLogo :: T.Text
organizationLogo = "https://acetalent.com/logo.svg"

socialLinks :: [T.Text]
socialLinks =
  [ "https://twitter.com/acetalent"
  , "https://linkedin.com/company/acetalent"
  , "https://instagram.com/acetalent"
  ]

-- | Tailwind configuration script
tailwindConfigScript :: T.Text
tailwindConfigScript = T.pack [istr|
tailwind.config = {
  theme: {
    extend: {
      colors: {
        primary: "#14a9db",
        secondary: "#2a89dc",
      },
    },
  },
};
|]



-- | Basic meta tags (charset, viewport, title)
buildBasicMeta :: DomBuilder t m => HeadConfig -> m ()
buildBasicMeta config = do
  elAttr "meta" ("charset" =: "UTF-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
  el "title" $ text (headTitle config)

-- | SEO meta tags
buildSeoMeta :: DomBuilder t m => HeadConfig -> m ()
buildSeoMeta config = do
  elAttr "meta" ("name" =: "description" <> "content" =: headDescription config) blank
  elAttr "meta" ("name" =: "keywords" <> "content" =: headKeywords config) blank
  elAttr "meta" ("name" =: "author" <> "content" =: siteName) blank
  elAttr "meta" ("name" =: "robots" <> "content" =: "index, follow") blank
  elAttr "meta" ("name" =: "language" <> "content" =: "English") blank
  elAttr "meta" ("name" =: "revisit-after" <> "content" =: "7 days") blank

-- | Open Graph meta tags
buildOpenGraphMeta :: DomBuilder t m => HeadConfig -> m ()
buildOpenGraphMeta config = do
  elAttr "meta" ("property" =: "og:title" <> "content" =: headOgTitle config) blank
  elAttr "meta" ("property" =: "og:description" <> "content" =: headOgDescription config) blank
  elAttr "meta" ("property" =: "og:image" <> "content" =: headOgImage config) blank
  elAttr "meta" ("property" =: "og:url" <> "content" =: headOgUrl config) blank
  elAttr "meta" ("property" =: "og:type" <> "content" =: "website") blank
  elAttr "meta" ("property" =: "og:site_name" <> "content" =: siteName) blank

-- | Twitter meta tags
buildTwitterMeta :: DomBuilder t m => HeadConfig -> m ()
buildTwitterMeta config = do
  elAttr "meta" ("name" =: "twitter:card" <> "content" =: "summary_large_image") blank
  elAttr "meta" ("name" =: "twitter:title" <> "content" =: headTwitterTitle config) blank
  elAttr "meta" ("name" =: "twitter:description" <> "content" =: headTwitterDescription config) blank
  elAttr "meta" ("name" =: "twitter:image" <> "content" =: headTwitterImage config) blank
  elAttr "meta" ("name" =: "twitter:site" <> "content" =: twitterHandle) blank

-- | Link tags (canonical, favicons, preconnects)
buildLinkTags :: DomBuilder t m => HeadConfig -> m ()
buildLinkTags config = do
  -- Canonical link
  elAttr "link" ("rel" =: "canonical" <> "href" =: headCanonicalUrl config) blank

  -- Favicons
  elAttr "link" ("rel" =: "icon" <> "type" =: "image/x-icon" <> "href" =: "/favicon.ico") blank
  elAttr "link" ("rel" =: "apple-touch-icon" <> "sizes" =: "180x180" <> "href" =: "/apple-touch-icon.webp") blank
  elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "sizes" =: "32x32" <> "href" =: "/favicon-32x32.webp") blank
  elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "sizes" =: "16x16" <> "href" =: "/favicon-16x16.webp") blank

  -- Preconnect links
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "") blank

  -- Conditional external stylesheets
  when (headIncludeSwiper config) $
    elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/swiper@11/swiper-bundle.min.css" <> "media" =: "print" <> "onload" =: "this.media='all'") blank

  when (headIncludeFontAwesome config) $
    elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css" <> "rel" =: "stylesheet" <> "media" =: "print" <> "onload" =: "this.media='all'") blank

-- | Structured data (JSON-LD)
buildStructuredData :: DomBuilder t m => HeadConfig -> m ()
buildStructuredData config = do
  let sameAs = T.intercalate ", " (map (\link_ -> "\"" <> link_ <> "\"") socialLinks)
  let jsonLd = T.pack $ [istr|
{
  "@context": "https://schema.org",
  "@type": "WebPage",
  "name": "#{headStructuredDataName config}",
  "description": #{headStructuredDataDescription config}",
  "url": "#{headStructuredDataUrl config}",
  "mainEntity": {
    "@type": "Organization",
    "name": "#{siteName}",
    "url": "https://acetalent.com",
    "logo": "#{organizationLogo}",
    "sameAs": #{sameAs}
  }
}
|]
  elAttr "script" ("type" =: "application/ld+json") $ text jsonLd

-- | Styles and scripts
buildStylesAndScripts :: DomBuilder t m => HeadConfig -> m ()
buildStylesAndScripts config = do
  -- Custom styles
  forM_ (headStyles config) $ \(CSS s) -> do
    el "style" $ text $ T.pack s

  -- Tailwind CSS (defer loading to avoid render-blocking)
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: $(static "css/styles.css")) blank
  -- <link rel="stylesheet" href="/static/css/styles.css" media="print" onload="this.media='all'">


  elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  el "script" $ text tailwindConfigScript

  -- Conditional external scripts (defer Swiper JS)
  when (headIncludeSwiper config) $
    elAttr "script" ("src" =: "https://cdn.jsdelivr.net/npm/swiper@11/swiper-bundle.min.js" <> "defer" =: "defer") blank
