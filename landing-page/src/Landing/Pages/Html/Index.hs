{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Landing.Pages.Html.Index where

import Landing.Static (static)
import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

indexHs :: DomBuilder t m => m ()
indexHs = indexHtml

indexHtml :: DomBuilder t m => m ()
indexHtml =
  elAttr "html" ("lang" =: "en") $ do  -- use <html> instead of <div lang=...>
    indexHead
    elAttr "body" ("class" =: "font-sans text-[#374151] bg-white") $ do
      navSection
      heroSection
      aboutSection
      servicesSection
      ctaSection
      contactSection
      footerSection
      scriptSection

indexHead :: DomBuilder t m => m ()
indexHead = el "head" $ do
  el "title" $ text "Locksmith Services - Gray Bruce"
  elAttr "meta"
    ("http-equiv" =: "Content-Type" <>
     "content" =: "text/html; charset=UTF-8") blank
  -- SEO: Provide a real meta description (required for best practices)
  elAttr "meta"
    ("name" =: "description" <>
     "content" =: "Gray Bruce Locksmith provides fast, reliable locksmith services including emergency lockout assistance, key duplication, lock installation, rekeying, and commercial security solutions in Georgian Bluffs, Owen Sound, and the entire Grey Bruce region. Over 20 years of trusted experience. Call 519-993-LOCK (5625) for your security needs.") blank
  elAttr "meta"
    ("name" =: "keywords" <>
     "content" =: "locksmith, emergency services, key duplication, lock installation, Gray Bruce, Owen Sound, home security, business security, rekeying, commercial locksmith, Georgian Bluffs, trusted locksmith, 24 hour locksmith, lock repair, high-security locks") blank
  elAttr "meta"
    ("property" =: "og:description" <>
     "content" =: "Gray Bruce Locksmith: expert lockout help, lock installation, rekeying, and more for homes and businesses with 20+ years experience.") blank
  elAttr "meta" ("name" =: "icbm" <> "content" =: "44.8438689,-81.1511791") blank
  elAttr "meta" ("name" =: "geo.position" <> "content" =: "44.8438689;-81.1511791") blank
  elAttr "meta" ("name" =: "geo.placename" <> "content" =: "Georgian Bluffs, Ontario") blank
  elAttr "meta" ("property" =: "og:title" <> "content" =: "Locksmith Services - Gray Bruce") blank
  elAttr "meta" ("property" =: "og:type" <> "content" =: "website") blank
  elAttr "meta" ("property" =: "og:locale" <> "content" =: "en_CA") blank
  elAttr "meta" ("property" =: "og:url" <> "content" =: "https://thelockguy.ca/") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank

  -- Essential favicon for SEO/UX
  elAttr "link" ("rel" =: "icon" <> "href" =: "data:,") blank
  elAttr "link" ("rel" =: "stylesheet" <> "href" =: $(static "css/styles.css")) blank
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
  elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com" <> "crossorigin" =: "anonymous") blank

  -- Stylesheets (add more as needed, e.g., for custom styles)
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "media" =: "screen" <> "href" =: "/webcard/static/app.min.1765894245.css") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "media" =: "screen" <> "href" =: "/css/custom.260111000213.css" <> "id" =: "customcss") blank

  -- Canonical link (important for SEO)
  elAttr "link" ("rel" =: "canonical" <> "href" =: "") blank

  -- Preload web fonts for performance
  elAttr "link" ("rel" =: "preload" <> "as" =: "style" <> "href" =: "/g/fonts.css?family=Open+Sans:400%7CMontserrat:400,700&display=swap") blank
  elAttr "link" ("rel" =: "stylesheet" <> "media" =: "all" <> "onload" =: "this.media='all'" <> "href" =: "/g/fonts.css?family=Open+Sans:400%7CMontserrat:400,700&display=swap") blank
  el "style" $ text $ T.unlines
    [ "html { scroll-behavior: smooth; }"
    ]

  elAttr "script" ("type" =: "application/ld+json") $ text $ T.unlines
    [ "{"
    , "  \"@context\": \"https://schema.org\","
    , "  \"@type\": \"Locksmith\","
    , "  \"name\": \"Gray Bruce Locksmith\","
    , "  \"url\": \"https://thelockguy.ca\","
    , "  \"telephone\": \"+1-519-993-5625\","
    , "  \"email\": \"thelockguy@yahoo.com\","
    , "  \"address\": {"
    , "    \"@type\": \"PostalAddress\","
    , "    \"streetAddress\": \"381067 Concession 17\","
    , "    \"addressLocality\": \"Georgian Bluffs\","
    , "    \"addressRegion\": \"ON\","
    , "    \"addressCountry\": \"CA\""
    , "  },"
    , "  \"areaServed\": {"
    , "    \"@type\": \"Place\","
    , "    \"name\": \"Gray Bruce\""
    , "  },"
    , "  \"description\": \"Your trusted local locksmith service in Gray Bruce providing emergency lockout assistance, key duplication, lock installation, and rekeying services.\","
    , "  \"openingHours\": \"Mo-Su 00:00-23:59\","
    , "  \"priceRange\": \"$$\","
    , "  \"hasOfferCatalog\": {"
    , "    \"@type\": \"OfferCatalog\","
    , "    \"name\": \"Locksmith Services\","
    , "    \"itemListElement\": ["
    , "      {\"@type\": \"Offer\", \"itemOffered\": {\"@type\": \"Service\", \"name\": \"Emergency Lockout Assistance\"}},"
    , "      {\"@type\": \"Offer\", \"itemOffered\": {\"@type\": \"Service\", \"name\": \"Key Duplication\"}},"
    , "      {\"@type\": \"Offer\", \"itemOffered\": {\"@type\": \"Service\", \"name\": \"Lock Installation for Homes\"}},"
    , "      {\"@type\": \"Offer\", \"itemOffered\": {\"@type\": \"Service\", \"name\": \"Lock Installation for Businesses\"}},"
    , "      {\"@type\": \"Offer\", \"itemOffered\": {\"@type\": \"Service\", \"name\": \"Rekeying Services\"}},"
    , "      {\"@type\": \"Offer\", \"itemOffered\": {\"@type\": \"Service\", \"name\": \"High-Security Lock Solutions\"}}"
    , "    ]"
    , "  }"
    , "}"
    ]

navSection :: DomBuilder t m => m ()
navSection =
  elAttr "nav" ("class" =: "fixed top-0 left-0 right-0 bg-[#0d1b2a] py-4 z-50 shadow-md") $
    elAttr "div" ("class" =: "max-w-6xl mx-auto px-8 flex flex-col md:flex-row justify-between items-center gap-4") $ do
      elAttr "a" ("href" =: "#home" <> "class" =: "text-2xl font-bold text-white hover:text-[#d69e2e] transition-colors") $
        text "thelockguy.ca"
      elAttr "ul" ("class" =: "flex flex-wrap justify-center gap-4 md:gap-8") $ do
        elClass "li" " md:block hidden" $ elAttr "a" linkCommonAttrsHome $ text "Home"
        elClass "li" " md:block hidden" $ elAttr "a" linkCommonAttrsAbout $ text "About Us"
        elClass "li" " md:block hidden" $ elAttr "a" linkCommonAttrsServices $ text "Services"
        elClass "li" " md:block hidden" $ elAttr "a" linkCommonAttrsCta $ text "Get Help"
        elClass "li" " md:block hidden" $ elAttr "a" linkCommonAttrsContact $ text "Contact"

heroSection :: DomBuilder t m => m ()
heroSection =
  elAttr "section" ("id" =: "home" <> "class" =: "relative min-h-screen flex items-center justify-center bg-gradient-to-br from-[#0d1b2a] to-[#1a365d] overflow-hidden") $ do
    elAttr "div" ("class" =: "absolute inset-0 bg-black/20") blank
    elAttr "div" ("class" =: "absolute inset-0 opacity-30" <> "style" =: "background-image: radial-gradient(circle at 20% 50%, rgba(255,255,255,0.05) 0%, transparent 50%), radial-gradient(circle at 80% 20%, rgba(255,255,255,0.03) 0%, transparent 40%), radial-gradient(circle at 40% 80%, rgba(255,255,255,0.04) 0%, transparent 45%);") blank
    elAttr "div" ("class" =: "relative z-10 text-center text-white px-8 max-w-4xl") $ do
      elAttr "h1" ("class" =: "text-4xl md:text-5xl lg:text-6xl font-bold mb-4 leading-tight") $
        text "Your Trusted Locksmith in Gray Bruce"
      -- 20 years in business badge
      elAttr "h2" ("class" =: "pt-4 text-xl md:text-2xl font-light mb-8 opacity-90") $
        text "Over 20 years of proudly delivering reliable solutions for all your lock needs"
      elAttr "a" ("href" =: "#contact" <> "class" =: "inline-block bg-[#d69e2e] text-[#1a365d] font-semibold px-8 py-4 rounded hover:bg-[#b7791f] hover:-translate-y-0.5 hover:shadow-lg transition-all mb-12") $
        text "Get Help Now"
      elAttr "div" ("class" =: "flex flex-col gap-3 mt-8") $ do
        elAttr "p" ("class" =: "flex items-center justify-center gap-2 text-lg") $ do
          elAttr "span" ("class" =: "text-xl") $ text "📍"
          text "381067 Concession 17, Georgian Bluffs, Ontario"
        elAttr "p" ("class" =: "flex items-center justify-center gap-2 text-lg") $ do
          elAttr "span" ("class" =: "text-xl") $ text "📞"
          text "519-993-LOCK (5625)"
        elAttr "p" ("class" =: "flex items-center justify-center gap-2 text-lg") $ do
          elAttr "span" ("class" =: "text-xl") $ text "✉️"
          elAttr "a" ("href" =: "mailto:thelockguy@yahoo.com" <> "class" =: "text-[#d69e2e] hover:underline") $
            text "thelockguy@yahoo.com"

aboutSection :: DomBuilder t m => m ()
aboutSection =
  elAttr "section" ("id" =: "about" <> "class" =: "py-24 bg-white") $
    elAttr "div" ("class" =: "max-w-6xl mx-auto px-8") $ do
      elAttr "h2" ("class" =: "text-3xl md:text-4xl font-bold text-[#0d1b2a] mb-8 text-center") $
        text "About Gray Bruce Locksmith"
      elAttr "div" ("class" =: "max-w-3xl mx-auto text-center") $ do
        elAttr "p" ("class" =: "text-lg text-[#4b5563] mb-6 leading-relaxed") $
          text "At Gray Bruce Locksmith, we are your trusted local locksmith service dedicated to providing reliable and prompt solutions for all your lock needs. Whether you require emergency lockout assistance, key duplication, or lock installation for your home or business, our professional team is here to help you feel secure."
        elAttr "p" ("class" =: "text-lg text-[#4b5563] mb-8 leading-relaxed") $
          text "With years of experience and dedication to our customers, we ensure that your safety is our priority."
        elAttr "a" ("href" =: "#services" <> "class" =: "inline-block border-2 border-[#1a365d] text-[#1a365d] font-semibold px-8 py-4 rounded hover:bg-[#1a365d] hover:text-white transition-all") $
          text "Learn More About Us"

servicesSection :: DomBuilder t m => m ()
servicesSection =
  elAttr "section" ("id" =: "services" <> "class" =: "py-24 bg-[#f9fafb]") $
    elAttr "div" ("class" =: "max-w-6xl mx-auto px-8") $ do
      elAttr "h2" ("class" =: "text-3xl md:text-4xl font-bold text-[#0d1b2a] mb-12 text-center") $
        text "Our Services"
      elAttr "div" ("class" =: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8") $ do
        serviceCard "🔓" "Emergency Lockout Assistance" "Fast response when you're locked out of your home, car, or business."
        serviceCard "🔑" "Key Duplication" "Precise copies of your keys for family, employees, or backup."
        serviceCard "🏠" "Lock Installation for Homes" "Secure your home with professional lock installation services."
        serviceCard "🏢" "Lock Installation for Businesses" "Commercial-grade security solutions for your business."
        serviceCard "🔄" "Rekeying Services" "Change your locks without replacing the hardware."
        serviceCard "🛡️" "High-Security Lock Solutions" "Advanced security systems for maximum protection."

ctaSection :: DomBuilder t m => m ()
ctaSection =
  elAttr "section" ("id" =: "cta" <> "class" =: "py-20 bg-gradient-to-br from-[#1a365d] to-[#0d1b2a] text-white text-center") $
    elAttr "div" ("class" =: "max-w-6xl mx-auto px-8") $ do
      elAttr "h2" ("class" =: "text-3xl md:text-4xl font-bold mb-4") $
        text "Need Immediate Lock Assistance?"
      elAttr "p" ("class" =: "text-xl opacity-90 mb-8") $
        text "We're just a call away for all your locksmith needs."
      elAttr "a" ("href" =: "#contact" <> "class" =: "inline-block bg-[#d69e2e] text-[#1a365d] font-semibold px-8 py-4 rounded hover:bg-[#b7791f] hover:-translate-y-0.5 hover:shadow-lg transition-all") $
        text "Contact Us"

contactSection :: DomBuilder t m => m ()
contactSection =
  elAttr "section" ("id" =: "contact" <> "class" =: "py-24 bg-white") $
    elAttr "div" ("class" =: "max-w-6xl mx-auto px-8") $ do
      elAttr "h2" ("class" =: "text-3xl md:text-4xl font-bold text-[#0d1b2a] mb-12 text-center") $
        text "Get in Touch With Us"
      elAttr "form" ("class" =: "max-w-xl mx-auto" <> "onsubmit" =: "handleSubmit(event)") $ do
        --gridCol Col2 $ do
        contactInput "text"    "name"    "Your Name"    True
        contactInput "email"   "email"   "Your Email"   True
        contactInput "tel"     "phone"   "Your Phone"   False
        contactTextArea
        contactCheckbox
        contactSubmitButton

contactInput :: DomBuilder t m => Text -> Text -> Text -> Bool -> m ()
contactInput t_ name ph req =
  elAttr "div" ("class" =: "mb-6") $
    elAttr "input"
      ( "type" =: t_
     <> "name" =: name
     <> "placeholder" =: ph
     <> (if req then "required" =: "required" else mempty)
     <> "class" =: "w-full px-4 py-4 border border-[#e5e7eb] rounded text-base focus:outline-none focus:border-[#1a365d] focus:ring-2 focus:ring-[#1a365d]/10 transition-all"
      ) blank

contactTextArea :: DomBuilder t m => m ()
contactTextArea =
  elAttr "div" ("class" =: "mb-6") $
    elAttr "textarea"
      ( "name" =: "message"
     <> "placeholder" =: "Your Message"
     <> "rows" =: "5"
     <> "required" =: "required"
     <> "class" =: "w-full px-4 py-4 border border-[#e5e7eb] rounded text-base resize-y min-h-32 focus:outline-none focus:border-[#1a365d] focus:ring-2 focus:ring-[#1a365d]/10 transition-all"
      ) $ text ""

contactCheckbox :: DomBuilder t m => m ()
contactCheckbox = do
  elAttr "div" ("class" =: "mb-6 flex items-center gap-3") $ do
    elAttr "input"
      ( "type" =: "checkbox"
     <> "id" =: "privacy"
     <> "name" =: "privacy"
     <> "required" =: "required"
     <> "class" =: "cursor-pointer"
      ) blank
    elAttr "label" ("for" =: "privacy" <> "class" =: "text-[#6b7280] cursor-pointer") $
      text "I have read and understand the privacy policy."

contactSubmitButton :: DomBuilder t m => m ()
contactSubmitButton =
  elAttr "button"
    ( "type" =: "submit"
   <> "class" =: "w-full bg-[#d69e2e] text-[#1a365d] font-semibold py-4 rounded text-lg hover:bg-[#b7791f] hover:-translate-y-0.5 hover:shadow-lg transition-all"
    ) $
      text "Submit Inquiry"

footerSection :: DomBuilder t m => m ()
footerSection =
  elAttr "footer" ("class" =: "bg-[#0d1b2a] text-white py-12") $
    elAttr "div" ("class" =: "max-w-6xl mx-auto px-8") $
      elAttr "div" ("class" =: "flex flex-col items-center gap-8") $ do
        elAttr "nav" ("class" =: "flex flex-wrap justify-center gap-8") $ do
          elAttr "a" ("href" =: "#home" <> "class" =: "opacity-80 hover:opacity-100 hover:text-[#d69e2e] transition-all") $ text "Home"
          elAttr "a" ("href" =: "#about" <> "class" =: "opacity-80 hover:opacity-100 hover:text-[#d69e2e] transition-all") $ text "About Us"
          elAttr "a" ("href" =: "#services" <> "class" =: "opacity-80 hover:opacity-100 hover:text-[#d69e2e] transition-all") $ text "Services"
          elAttr "a" ("href" =: "#contact" <> "class" =: "opacity-80 hover:opacity-100 hover:text-[#d69e2e] transition-all") $ text "Contact"
        elAttr "div" mempty $
          elAttr "h3" ("class" =: "text-2xl font-bold") $ text "thelockguy.ca"
        elAttr "div" ("class" =: "text-sm opacity-70") $ do
          elAttr "a" ("href" =: "/legal-notice" <> "class" =: "hover:text-[#d69e2e] transition-colors") $ text "Legal Notice"
          elAttr "span" ("class" =: "mx-2") $ text "|"
          elAttr "a" ("href" =: "/privacy" <> "class" =: "hover:text-[#d69e2e] transition-colors") $ text "Privacy Policy"

scriptSection :: DomBuilder t m => m ()
scriptSection =
  el "script" $
    text $ T.unlines
      [ "function handleSubmit(event) {"
      , "  event.preventDefault();"
      , "  const formData = new FormData(event.target);"
      , "  const data = Object.fromEntries(formData);"
      , "  fetch('/email', {"
      , "    method: 'POST',"
      , "    headers: {"
      , "      'Content-Type': 'application/json'"
      , "    },"
      , "    body: JSON.stringify(data)"
      , "  })"
      , "  .then(response => {"
      , "    if (!response.ok) throw new Error('Network response was not ok');"
      , "    return response.json().catch(() => ({}));"
      , "  })"
      , "  .then(() => {"
      , "    alert('Thank you for your inquiry! We will get back to you soon.');"
      , "    event.target.reset();"
      , "  })"
      , "  .catch((error) => {"
      , "    alert('There was an error submitting the form. Please try again later.');"
      , "    console.error('Submission error:', error);"
      , "  });"
      , "}"
      ]

linkBaseClasses :: Text
linkBaseClasses = "text-white font-medium hover:text-[#d69e2e] transition-colors relative after:absolute after:bottom-0 after:left-0 after:w-0 after:h-0.5 after:bg-[#d69e2e] after:transition-all hover:after:w-full"

linkCommonAttrsHome :: Map.Map Text Text
linkCommonAttrsHome     = "href" =: "#home"    <> "class" =: linkBaseClasses

linkCommonAttrsAbout :: Map.Map Text Text
linkCommonAttrsAbout    = "href" =: "#about"   <> "class" =: linkBaseClasses

linkCommonAttrsServices :: Map.Map Text Text
linkCommonAttrsServices = "href" =: "#services"<> "class" =: linkBaseClasses

linkCommonAttrsCta :: Map.Map Text Text
linkCommonAttrsCta      = "href" =: "#cta"     <> "class" =: linkBaseClasses

linkCommonAttrsContact :: Map.Map Text Text
linkCommonAttrsContact  = "href" =: "#contact" <> "class" =: linkBaseClasses

serviceCard :: DomBuilder t m => Text -> Text -> Text -> m ()
serviceCard icon title body =
  elAttr "div" ("class" =: "bg-white p-10 rounded-lg shadow hover:-translate-y-2 hover:shadow-xl transition-all text-center") $ do
    elAttr "div" ("class" =: "text-5xl mb-6") $ text icon
    elAttr "h3" ("class" =: "text-xl font-bold text-[#0d1b2a] mb-4") $ text title
    elAttr "p" ("class" =: "text-[#6b7280] leading-relaxed") $ text body

