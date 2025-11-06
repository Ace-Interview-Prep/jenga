module Landing.Pages.Html.Scripts where


import Text.IStr
import Data.Some (Some(..))

-- A simple indexed GADT
data Value' a where
  VInt    :: Int    -> Value' Int
  VBool   :: Bool   -> Value' Bool
  VString :: String -> Value' String

deriving instance Show (Value' a)

-- Wrap examples into Some
store :: [Some Value']
store =
  [ Some (VInt 1)
  , Some (VBool False)
  , Some (VString "hi")
  ]

-- Apply one "step" to each case and return a *String* as result
step :: Some Value' -> String
step (Some v) = case v of
  VInt n    -> "incremented: " ++ show (n + 1)
  VBool b   -> "negated: " ++ show (not b)
  VString s -> "shouted: " ++ map toUpper s
  where
    toUpper c
      | 'a' <= c && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise            = c

-- main :: IO ()
-- main = mapM_ (putStrLn . step) store

newtype Script = Script String deriving (Semigroup, Monoid)



data JSObject = JSObject String
-- | Collect a JS reference and do something with it
-- | Enforces referential transparency is the purpose
data SyntaxJS a where
  With :: SyntaxJS (JSObject, (JSObject -> a))
  -- WithAPI :: ?



-- Common scripts for all pages

commonHeaderScrollScript :: Script
commonHeaderScrollScript = Script [istr|
  // Header scroll effect (common)
  window.addEventListener("scroll", function () {
    const header = document.getElementById("header");
    if (!header) {
      // Page-specific: log with page name if needed
      return;
    }
    if (window.scrollY > 50) {
      header.classList.remove("bg-transparent", "pt-6");
      header.classList.add("bg-white/5", "backdrop-blur-sm", "pt-3");
    } else {
      header.classList.remove("bg-white/5", "backdrop-blur-sm", "pt-3");
      header.classList.add("bg-transparent", "pt-6");
    }
  });
|]

commonMobileMenuScript :: Script
commonMobileMenuScript = Script [istr|
  // Mobile menu functionality (common)
  const mobileMenuBtn = document.getElementById("mobile-menu-btn");
  const mobileMenu = document.getElementById("mobile-menu");
  const backdrop = document.getElementById("backdrop");
  const hamburgerIcon = document.getElementById("hamburger-icon");
  const closeIcon = document.getElementById("close-icon");
  let isMenuOpen = false;

  function openMobileMenu() {
    isMenuOpen = true;
    if (backdrop) {
      backdrop.classList.remove("hidden");
      backdrop.classList.add("menu-backdrop");
    }
    if (mobileMenu) {
      mobileMenu.classList.remove("hidden");
      mobileMenu.classList.add("mobile-menu-enter");
    }
    if (hamburgerIcon) hamburgerIcon.classList.add("hidden");
    if (closeIcon) closeIcon.classList.remove("hidden");
  }

  function closeMobileMenu() {
    if (backdrop) backdrop.classList.add("menu-backdrop-exit");
    if (mobileMenu) mobileMenu.classList.add("mobile-menu-exit");
    setTimeout(() => {
      isMenuOpen = false;
      if (backdrop) {
        backdrop.classList.add("hidden");
        backdrop.classList.remove("menu-backdrop", "menu-backdrop-exit");
      }
      if (mobileMenu) {
        mobileMenu.classList.add("hidden");
        mobileMenu.classList.remove("mobile-menu-enter", "mobile-menu-exit");
      }
      if (hamburgerIcon) hamburgerIcon.classList.remove("hidden");
      if (closeIcon) closeIcon.classList.add("hidden");
    }, 300);
  }

  if (mobileMenuBtn) {
    mobileMenuBtn.addEventListener("click", function () {
      if (isMenuOpen) {
        closeMobileMenu();
      } else {
        openMobileMenu();
      }
    });
  }
  if (backdrop) {
    backdrop.addEventListener("click", closeMobileMenu);
  }
|]

commonSwiperScript :: Script
commonSwiperScript = Script [istr|
  // Swiper for testimonials (common)
  function initTestimonialSwiper() {
    if (typeof Swiper !== "undefined") {
      new Swiper(".testimonial-swiper", {
        spaceBetween: 24,
        breakpoints: {
          640: { slidesPerView: 1 },
          768: { slidesPerView: 2 },
        },
      });
    }
  }
  if (typeof Swiper !== "undefined") {
    initTestimonialSwiper();
  } else {
    window.addEventListener("load", initTestimonialSwiper);
  }
|]

commonFadeInObserverScript :: Script
commonFadeInObserverScript = Script [istr|
  // Intersection Observer for fade-in animations (common)
  const observerOptions = {
    threshold: 0.1,
    rootMargin: "0px 0px -50px 0px",
  };
  if ("IntersectionObserver" in window) {
    const observer = new IntersectionObserver(function (entries) {
      entries.forEach((entry) => {
        if (entry.isIntersecting) {
          entry.target.classList.add("is-visible");
        }
      });
    }, observerOptions);
    document.querySelectorAll(".fade-in-section").forEach((section) => {
      observer.observe(section);
    });
  } else {
    // Fallback: reveal all
    document.querySelectorAll(".fade-in-section").forEach((s) => s.classList.add("is-visible"));
  }
|]

-- Page-specific scripts

aboutUsScript :: Script
aboutUsScript = mconcat
  [ commonHeaderScrollScript
  , commonMobileMenuScript
  , commonSwiperScript
  , commonFadeInObserverScript
  , Script [istr|
      // About Us: Counter animation for statistics
      function animateCounter(element) {
        const targetAttr = element.getAttribute("data-target") || "0";
        const target = parseInt(targetAttr, 10);
        if (isNaN(target)) return;
        const duration = 2000;
        const increment = target / (duration / 16);
        let current = 0;
        const timer = setInterval(() => {
          current += increment;
          if (current >= target) {
            current = target;
            clearInterval(timer);
          }
          element.textContent = Math.floor(current).toLocaleString();
        }, 16);
      }
      // Animate counters when fade-in-section is visible
      if ("IntersectionObserver" in window) {
        const observer = new IntersectionObserver((entries) => {
          entries.forEach((entry) => {
            if (entry.isIntersecting) {
              const counters = entry.target.querySelectorAll(".counter");
              counters.forEach((counter) => {
                if (counter.dataset.aceAnimated !== "true") {
                  counter.dataset.aceAnimated = "true";
                  animateCounter(counter);
                }
              });
            }
          });
        }, { threshold: 0.1, rootMargin: "0px 0px -50px 0px" });
        document.querySelectorAll(".fade-in-section").forEach((section) => observer.observe(section));
      } else {
        document.querySelectorAll(".counter").forEach((c) => {
          if (c.dataset.aceAnimated !== "true") {
            c.dataset.aceAnimated = "true";
            animateCounter(c);
          }
        });
      }

      // About Us: Video play button
      const videoBtn = document.querySelector(".video-play-btn");
      if (videoBtn) {
        videoBtn.addEventListener("click", () => {
          // Hook up your player integration here
        });
      }
    |]
  ]

blogScript :: Script
blogScript = mconcat
  [ commonHeaderScrollScript
  , commonMobileMenuScript
  , commonSwiperScript
  , commonFadeInObserverScript
  , Script [istr|
      // Blog: Testimonial navigation buttons
      const testimonialSwiper = document.querySelector('.testimonial-swiper') && typeof Swiper !== "undefined" ? new Swiper(".testimonial-swiper") : null;
      const testimonialPrev = document.getElementById("testimonial-prev");
      const testimonialNext = document.getElementById("testimonial-next");
      if (testimonialPrev && testimonialNext && testimonialSwiper) {
        testimonialPrev.addEventListener("click", () => testimonialSwiper.slidePrev());
        testimonialNext.addEventListener("click", () => testimonialSwiper.slideNext());
      }

      // Blog: Card interactions
      document.querySelectorAll(".blog-title").forEach((title) => {
        title.addEventListener("click", function () {
          // Add your blog navigation logic here
        });
      });
      document.querySelectorAll(".read-more-btn").forEach((button) => {
        button.addEventListener("click", function () {
          // Add your read more functionality here
        });
      });
      document.querySelectorAll(".blog-tag").forEach((tag) => {
        tag.addEventListener("click", function () {
          // Add your tag filtering logic here
        });
      });

      // Blog: Button click handlers
      document.querySelectorAll(".btn-primary").forEach((button) => {
        button.addEventListener("click", function () {
          // Add your button functionality here
        });
      });

      // Blog: Staggered animation to blog cards
      document.querySelectorAll(".blog-card-animate").forEach((card, index) => {
        card.style.animationDelay = `${index * 0.1}s`;
      });

      // Blog: Hover effects for blog cards
      document.querySelectorAll(".blog-card").forEach((card) => {
        card.addEventListener("mouseenter", function () {
          this.style.transform = "translateY(-5px)";
        });
        card.addEventListener("mouseleave", function () {
          this.style.transform = "translateY(0)";
        });
      });
      document.querySelectorAll(".featured-blog-card").forEach((card) => {
        card.addEventListener("mouseenter", function () {
          this.style.transform = "translateY(-3px)";
        });
        card.addEventListener("mouseleave", function () {
          this.style.transform = "translateY(0)";
        });
      });
    |]
  ]

contactScript :: Script
contactScript = mconcat
  [ commonHeaderScrollScript
  , commonMobileMenuScript
  , commonFadeInObserverScript
  , Script [istr|
      // Contact: Form validation and submission
      const contactForm = document.getElementById("contact-form");
      const submitBtn = document.getElementById("submit-btn");
      const submitText = document.getElementById("submit-text");
      const loadingSpinner = document.getElementById("loading-spinner");
      const submitArrow = document.getElementById("submit-arrow");
      const formMessage = document.getElementById("form-message");
      function validateField(field) {
        const value = field.value.trim();
        const fieldName = field.name;
        const errorElement = document.getElementById(`${fieldName}-error`);
        let isValid = true;
        let errorMessage = "";
        field.classList.remove("field-valid", "field-invalid");
        errorElement.classList.remove("show");
        switch (fieldName) {
          case "name":
            if (value.length < 2) { isValid = false; errorMessage = "Name must be at least 2 characters long"; }
            break;
          case "phone":
            const phoneRegex = /^[\+]?[1-9][\d]{0,15}$/;
            if (!phoneRegex.test(value.replace(/\s/g, ""))) { isValid = false; errorMessage = "Please enter a valid phone number"; }
            break;
          case "email":
            const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
            if (!emailRegex.test(value)) { isValid = false; errorMessage = "Please enter a valid email address"; }
            break;
          case "message":
            if (value.length < 10) { isValid = false; errorMessage = "Message must be at least 10 characters long"; }
            break;
        }
        if (isValid) {
          field.classList.add("field-valid");
        } else {
          field.classList.add("field-invalid");
          errorElement.textContent = errorMessage;
          errorElement.classList.add("show");
        }
        return isValid;
      }
      const formFields = ["name", "phone", "email", "message"];
      formFields.forEach((fieldName) => {
        const field = document.getElementById(fieldName);
        field.addEventListener("blur", () => validateField(field));
        field.addEventListener("input", () => {
          if (field.classList.contains("field-invalid")) validateField(field);
        });
        field.addEventListener("focus", () => field.classList.add("form-field-focus"));
        field.addEventListener("blur", () => field.classList.remove("form-field-focus"));
      });
      contactForm.addEventListener("submit", async function (e) {
        e.preventDefault();
        let isFormValid = true;
        formFields.forEach((fieldName) => {
          const field = document.getElementById(fieldName);
          if (!validateField(field)) isFormValid = false;
        });
        if (!isFormValid) {
          submitBtn.classList.add("form-error");
          setTimeout(() => submitBtn.classList.remove("form-error"), 500);
          return;
        }
        submitBtn.disabled = true;
        submitText.textContent = "Submitting...";
        loadingSpinner.classList.remove("hidden");
        submitArrow.classList.add("hidden");
        try {
          await new Promise((resolve) => setTimeout(resolve, 2000));
          formMessage.className = "text-center p-4 rounded-2xl bg-green-50 text-green-700 form-success";
          formMessage.textContent = "Thank you! Your message has been sent successfully. We'll get back to you soon.";
          formMessage.classList.remove("hidden");
          contactForm.reset();
          formFields.forEach((fieldName) => {
            const field = document.getElementById(fieldName);
            field.classList.remove("field-valid", "field-invalid");
            const errorElement = document.getElementById(`${fieldName}-error`);
            errorElement.classList.remove("show");
          });
          setTimeout(() => { formMessage.classList.add("hidden"); }, 5000);
        } catch (error) {
          formMessage.className = "text-center p-4 rounded-2xl bg-red-50 text-red-700 form-error";
          formMessage.textContent = "Sorry, there was an error sending your message. Please try again.";
          formMessage.classList.remove("hidden");
          setTimeout(() => { formMessage.classList.add("hidden"); }, 5000);
        } finally {
          submitBtn.disabled = false;
          submitText.textContent = "Submit Now";
          loadingSpinner.classList.add("hidden");
          submitArrow.classList.remove("hidden");
        }
      });

      // Contact: Social icon hover
      document.querySelectorAll(".social-hover").forEach((icon) => {
        icon.addEventListener("mouseenter", function () {
          this.style.transform = "translateY(-3px) scale(1.1)";
        });
        icon.addEventListener("mouseleave", function () {
          this.style.transform = "translateY(0) scale(1)";
        });
      });

      // Contact: Shimmer effect on submit button
      submitBtn.addEventListener("mouseenter", function () {
        this.classList.add("button-shimmer");
      });

      // Contact: Phone number formatting
      const phoneField = document.getElementById("phone");
      phoneField.addEventListener("input", function (e) {
        let value = e.target.value.replace(/\D/g, "");
        if (value.length > 0) {
          if (value.length <= 3) {
            value = `(${value}`;
          } else if (value.length <= 6) {
            value = `(${value.slice(0, 3)}) ${value.slice(3)}`;
          } else {
            value = `(${value.slice(0, 3)}) ${value.slice(3, 6)}-${value.slice(6, 10)}`;
          }
        }
        e.target.value = value;
      });

      // Contact: Typing animation effect to h2
      const titleElement = document.querySelector("h2");
      if (titleElement) {
        const originalText = titleElement.textContent;
        titleElement.textContent = "";
        setTimeout(() => {
          let i = 0;
          const typeWriter = () => {
            if (i < originalText.length) {
              titleElement.textContent += originalText.charAt(i);
              i++;
              setTimeout(typeWriter, 50);
            }
          };
          typeWriter();
        }, 500);
      }
    |]
  ]


engineerScript :: Script
engineerScript =
    let
      skill1_png, skill2_png, skill3_png :: String
      skill1_png = ""
      skill2_png = ""
      skill3_png = ""
    in Script [istr|
      // -------------------------------------
      // Header scroll effect
      // -------------------------------------
      window.addEventListener("scroll", function () {
        const header = document.getElementById("header");
        if (!header) {
          console.warn("[Engineer] Header element not found for scroll effect.");
          return;
        }
        if (window.scrollY > 50) {
          header.classList.remove("bg-transparent", "pt-6");
          header.classList.add("bg-white/5", "backdrop-blur-sm", "pt-3");
          console.log("[Engineer] Header scrolled: blur and background added.");
        } else {
          header.classList.remove("bg-white/5", "backdrop-blur-sm", "pt-3");
          header.classList.add("bg-transparent", "pt-6");
          console.log("[Engineer] Header at top: transparent background restored.");
        }
      });

      // -------------------------------------
      // Mobile menu functionality
      // -------------------------------------
      const mobileMenuBtn = document.getElementById("mobile-menu-btn");
      const mobileMenu = document.getElementById("mobile-menu");
      const backdrop = document.getElementById("backdrop");
      const hamburgerIcon = document.getElementById("hamburger-icon");
      const closeIcon = document.getElementById("close-icon");
      let isMenuOpen = false;

      function openMobileMenu() {
        isMenuOpen = true;
        backdrop.classList.remove("hidden");
        backdrop.classList.add("menu-backdrop");
        mobileMenu.classList.remove("hidden");
        mobileMenu.classList.add("mobile-menu-enter");
        hamburgerIcon.classList.add("hidden");
        closeIcon.classList.remove("hidden");
        console.log("[Engineer] Mobile menu opened.");
      }

      function closeMobileMenu() {
        backdrop.classList.add("menu-backdrop-exit");
        mobileMenu.classList.add("mobile-menu-exit");
        setTimeout(() => {
          isMenuOpen = false;
          backdrop.classList.add("hidden");
          backdrop.classList.remove("menu-backdrop", "menu-backdrop-exit");
          mobileMenu.classList.add("hidden");
          mobileMenu.classList.remove("mobile-menu-enter", "mobile-menu-exit");
          hamburgerIcon.classList.remove("hidden");
          closeIcon.classList.add("hidden");
          console.log("[Engineer] Mobile menu closed.");
        }, 300);
      }

      if (mobileMenuBtn) {
        mobileMenuBtn.addEventListener("click", function () {
          if (isMenuOpen) {
            closeMobileMenu();
          } else {
            openMobileMenu();
          }
        });
        console.log("[Engineer] Mobile menu button handler attached.");
      } else {
        console.warn("[Engineer] Mobile menu button not found.");
      }

      // Optionally enable backdrop click to close menu
      // if (backdrop) {
      //   backdrop.addEventListener("click", closeMobileMenu);
      //   console.log("[Engineer] Backdrop click handler attached.");
      // } else {
      //   console.warn("[Engineer] Backdrop element not found.");
      // }

      // -------------------------------------
      // Skills tab functionality
      // -------------------------------------
      const skillsData = {
        communication: {
          title: "Communication Skills Training",
          description:
            "Ace Interview Prep helps candidates master storytelling, insightful questioning, and collaboration. With mock interviews and AI-driven feedback on 20+ soft skills, they gain actionable insights to excel in both technical and communication skills.",
          image: "#{skill1_png :: String}",
          icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>`,
        },
        programming: {
          title: "Functional Programming Mastery",
          description:
            "Our comprehensive functional programming curriculum covers advanced concepts, best practices, and real-world applications. Candidates learn through hands-on projects and receive personalized feedback to master complex programming paradigms.",
          image: "#{skill2_png :: String}",
          icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"></path>`,
        },
        cultural: {
          title: "Cultural Adaptability Development",
          description:
            "We focus on developing cultural intelligence and adaptability skills essential for global teams. Through immersive scenarios and cross-cultural communication training, candidates learn to thrive in diverse work environments.",
          image: "#{skill3_png}",
          icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"></path>`,
        },
      };

      const skillTabs = document.querySelectorAll(".skill-tab");
      const skillContent = document.getElementById("skill-content");
      const skillIcon = document.getElementById("skill-icon");
      const skillTitle = document.getElementById("skill-title");
      const skillDescription = document.getElementById("skill-description");
      const skillImage = document.getElementById("skill-image");

      skillTabs.forEach((tab) => {
        tab.addEventListener("click", function () {
          const tabId = this.getAttribute("data-tab");
          const skill = skillsData[tabId];
          console.log(`[Engineer] Skill tab clicked: ${tabId}`);

          // Update active tab
          skillTabs.forEach((t) => {
            t.classList.remove("active", "text-gray-900", "font-medium");
            t.classList.add("text-gray-500", "hover:text-gray-700");
          });
          this.classList.remove("text-gray-500", "hover:text-gray-700");
          this.classList.add("active", "text-gray-900", "font-medium");

          // Update content with animation
          skillContent.classList.remove("animate-slide-fade");
          setTimeout(() => {
            skillIcon.innerHTML = skill.icon;
            skillTitle.textContent = skill.title;
            skillDescription.textContent = skill.description;
            skillImage.src = skill.image;
            skillImage.alt = skill.title;
            skillContent.classList.add("animate-slide-fade");
            console.log(`[Engineer] Skill content updated: ${skill.title}`);
          }, 100);
        });
      });
      console.log("[Engineer] Skill tab handlers attached.");

      // -------------------------------------
      // Button click handlers (primary buttons)
      // -------------------------------------
      document.querySelectorAll(".btn-primary").forEach((button) => {
        button.addEventListener("click", function () {
          const buttonText = this.textContent.trim();
          console.log("[Engineer] Button clicked:", buttonText);
          // Add your button functionality here
        });
      });
      console.log("[Engineer] Primary button click handlers attached.");

      // -------------------------------------
      // Intersection Observer for fade-in animations
      // -------------------------------------
      const observerOptions = {
        threshold: 0.1,
        rootMargin: "0px 0px -50px 0px",
      };

      const observer = new IntersectionObserver(function (entries) {
        entries.forEach((entry) => {
          if (entry.isIntersecting) {
            entry.target.classList.add("is-visible");
            console.log("[Engineer] Fade-in section became visible:", entry.target);

            // Trigger feature item animations
            const featureItems = entry.target.querySelectorAll(".feature-item");
            featureItems.forEach((item, index) => {
              setTimeout(() => {
                item.style.opacity = "1";
                item.style.transform = "translateX(0)";
                console.log(`[Engineer] Feature item animated (index ${index})`, item);
              }, index * 100);
            });
          }
        });
      }, observerOptions);

      // Observe all fade-in sections
      document.querySelectorAll(".fade-in-section").forEach((section) => {
        observer.observe(section);
      });
      console.log("[Engineer] IntersectionObserver attached to fade-in sections.");

      // -------------------------------------
      // Add floating animation to globe on scroll
      // -------------------------------------
      let ticking = false;
      function updateGlobeAnimation() {
        const scrolled = window.pageYOffset;
        const parallax = scrolled * 0.5;
        const globeImage = document.querySelector(".globe-image");
        if (globeImage) {
          globeImage.style.transform = `translateY(${parallax}px)`;
          console.log("[Engineer] Globe image parallax updated:", parallax);
        }
        ticking = false;
      }

      function requestTick() {
        if (!ticking) {
          requestAnimationFrame(updateGlobeAnimation);
          ticking = true;
        }
      }

      window.addEventListener("scroll", requestTick);
      console.log("[Engineer] Globe parallax scroll handler attached.");

      // -------------------------------------
      // Add hover effects for images
      // -------------------------------------
      document.querySelectorAll(".hero-image").forEach((image) => {
        image.addEventListener("mouseenter", function () {
          this.style.transform = "scale(1.02)";
          console.log("[Engineer] Hero image hovered:", this);
        });

        image.addEventListener("mouseleave", function () {
          this.style.transform = "scale(1)";
          console.log("[Engineer] Hero image unhovered:", this);
        });
      });
      console.log("[Engineer] Hover effects attached to .hero-image elements.");
|]


engineerScript_2 :: Script
engineerScript_2 =
  let
    skill1_png, skill2_png, skill3_png :: String
    skill1_png = ""
    skill2_png = ""
    skill3_png = ""
  in mconcat
    [ commonHeaderScrollScript
    , commonMobileMenuScript
    , commonFadeInObserverScript
    , Script [istr|
        // Engineer: Skills tab functionality
        const skillsData = {
          communication: {
            title: "Communication Skills Training",
            description:
              "Ace Interview Prep helps candidates master storytelling, insightful questioning, and collaboration. With mock interviews and AI-driven feedback on 20+ soft skills, they gain actionable insights to excel in both technical and communication skills.",
            image: "#{skill1_png}",
            icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>`,
          },
          programming: {
            title: "Functional Programming Mastery",
            description:
              "Our comprehensive functional programming curriculum covers advanced concepts, best practices, and real-world applications. Candidates learn through hands-on projects and receive personalized feedback to master complex programming paradigms.",
            image: "#{skill2_png}",
            icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"></path>`,
          },
          cultural: {
            title: "Cultural Adaptability Development",
            description:
              "We focus on developing cultural intelligence and adaptability skills essential for global teams. Through immersive scenarios and cross-cultural communication training, candidates learn to thrive in diverse work environments.",
            image: "#{skill3_png}",
            icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"></path>`,
          },
        };
        const skillTabs = document.querySelectorAll(".skill-tab");
        const skillContent = document.getElementById("skill-content");
        const skillIcon = document.getElementById("skill-icon");
        const skillTitle = document.getElementById("skill-title");
        const skillDescription = document.getElementById("skill-description");
        const skillImage = document.getElementById("skill-image");
        skillTabs.forEach((tab) => {
          tab.addEventListener("click", function () {
            const tabId = this.getAttribute("data-tab");
            const skill = skillsData[tabId];
            skillTabs.forEach((t) => {
              t.classList.remove("active", "text-gray-900", "font-medium");
              t.classList.add("text-gray-500", "hover:text-gray-700");
            });
            this.classList.remove("text-gray-500", "hover:text-gray-700");
            this.classList.add("active", "text-gray-900", "font-medium");
            skillContent.classList.remove("animate-slide-fade");
            setTimeout(() => {
              skillIcon.innerHTML = skill.icon;
              skillTitle.textContent = skill.title;
              skillDescription.textContent = skill.description;
              skillImage.src = skill.image;
              skillImage.alt = skill.title;
              skillContent.classList.add("animate-slide-fade");
            }, 100);
          });
        });

        // Engineer: Button click handlers
        document.querySelectorAll(".btn-primary").forEach((button) => {
          button.addEventListener("click", function () {
            // Add your button functionality here
          });
        });

        // Engineer: Feature item animation on fade-in
        const observerOptions = { threshold: 0.1, rootMargin: "0px 0px -50px 0px" };
        const observer = new IntersectionObserver(function (entries) {
          entries.forEach((entry) => {
            if (entry.isIntersecting) {
              const featureItems = entry.target.querySelectorAll(".feature-item");
              featureItems.forEach((item, index) => {
                setTimeout(() => {
                  item.style.opacity = "1";
                  item.style.transform = "translateX(0)";
                }, index * 100);
              });
            }
          });
        }, observerOptions);
        document.querySelectorAll(".fade-in-section").forEach((section) => {
          observer.observe(section);
        });

        // Engineer: Globe parallax on scroll
        let ticking = false;
        function updateGlobeAnimation() {
          const scrolled = window.pageYOffset;
          const parallax = scrolled * 0.5;
          const globeImage = document.querySelector(".globe-image");
          if (globeImage) {
            globeImage.style.transform = `translateY(${parallax}px)`;
          }
          ticking = false;
        }
        function requestTick() {
          if (!ticking) {
            requestAnimationFrame(updateGlobeAnimation);
            ticking = true;
          }
        }
        window.addEventListener("scroll", requestTick);

        // Engineer: Hover effects for images
        document.querySelectorAll(".hero-image").forEach((image) => {
          image.addEventListener("mouseenter", function () {
            this.style.transform = "scale(1.02)";
          });
          image.addEventListener("mouseleave", function () {
            this.style.transform = "scale(1)";
          });
        });
      |]
    ]

faqScript :: Script
faqScript = mconcat
  [ commonHeaderScrollScript
  , commonMobileMenuScript
  , commonFadeInObserverScript
  , Script [istr|
// FAQ: Accordion functionality
document.querySelectorAll(".accordion-header").forEach((header) => {
  header.addEventListener("click", () => {
    const item = header.parentElement;
    const isOpen = item.classList.contains("open");
    document.querySelectorAll(".accordion-item").forEach((i) => {
      i.classList.remove("open");
    });
    if (!isOpen) {
      item.classList.add("open");
    }
  });
});

// FAQ: Page load animations
document.addEventListener("DOMContentLoaded", function () {
  setTimeout(() => {
    const heroElements = document.querySelectorAll(".animate-on-load");
    heroElements.forEach((el) => {
      el.classList.add("animate-fade-in-up");
    });
  }, 300);
});

// FAQ: Button click handlers
document.querySelectorAll(".btn-primary").forEach((button) => {
  button.addEventListener("click", function () {
    const buttonText = this.textContent.trim();
    if (buttonText === "Start Hiring") {
      window.location.href = "/signup";
    }
  });
});

// FAQ: Analytics tracking
function trackEvent(eventName, properties = {}) {
  // Implement your analytics tracking here
}
document.querySelectorAll(".accordion-header").forEach((header, index) => {
  header.addEventListener("click", function () {
    const question = this.querySelector("span") ? this.querySelector("span").textContent : this.textContent.trim();
    trackEvent("faq_question_clicked", {
      question: question,
      index: index,
    });
  });
});
trackEvent("page_view", {
  page: "faq",
  timestamp: new Date().toISOString(),
});
    |]
  ]

indexScript :: Script
indexScript =
  let
    skill1_png, skill2_png, skill3_png :: String
    skill1_png = ""
    skill2_png = ""
    skill3_png = ""
  in mconcat
    [ commonHeaderScrollScript
    , commonMobileMenuScript
    , commonSwiperScript
    , commonFadeInObserverScript
    , Script [istr|
        // Index: Skills tab functionality
        const skillsData = {
          communication: {
            title: "Communication Skills Training",
            description:
              "Ace Interview Prep helps candidates master storytelling, insightful questioning, and collaboration. With mock interviews and AI-driven feedback on 20+ soft skills, they gain actionable insights to excel in both technical and communication skills.",
            image: "#{ skill1_png }",
            icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3.055 11H5a2 2 0 012 2v1a2 2 0 002 2 2 2 0 012 2v2.945M8 3.935V5.5A2.5 2.5 0 0010.5 8h.5a2 2 0 012 2 2 2 0 104 0 2 2 0 012-2h1.064M15 20.488V18a2 2 0 012-2h3.064M21 12a9 9 0 11-18 0 9 9 0 0118 0z"></path>`,
          },
          programming: {
            title: "Functional Programming Mastery",
            description:
              "Our comprehensive functional programming curriculum covers advanced concepts, best practices, and real-world applications. Candidates learn through hands-on projects and receive personalized feedback to master complex programming paradigms.",
            image: "#{skill2_png}",
            icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"></path>`,
          },
          cultural: {
            title: "Cultural Adaptability Development",
            description:
              "We focus on developing cultural intelligence and adaptability skills essential for global teams. Through immersive scenarios and cross-cultural communication training, candidates learn to thrive in diverse work environments.",
            image: "#{skill3_png}",
            icon: `<path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"></path>`,
          },
        };
        const skillTabs = document.querySelectorAll(".skill-tab");
        const skillContent = document.getElementById("skill-content");
        const skillIcon = document.getElementById("skill-icon");
        const skillTitle = document.getElementById("skill-title");
        const skillDescription = document.getElementById("skill-description");
        const skillImage = document.getElementById("skill-image");
        skillTabs.forEach((tab) => {
          tab.addEventListener("click", function () {
            const tabId = this.getAttribute("data-tab");
            const skill = skillsData[tabId];
            skillTabs.forEach((t) => {
              t.classList.remove(
                "text-gray-900",
                "font-medium",
                "border-b-2",
                "border-[#14a9db]"
              );
              t.classList.add("text-gray-500", "hover:text-gray-700");
            });
            this.classList.remove("text-gray-500", "hover:text-gray-700");
            this.classList.add(
              "text-gray-900",
              "font-medium",
              "border-b-2",
              "border-[#14a9db]"
            );
            skillContent.classList.remove("animate-slide-fade");
            setTimeout(() => {
              skillIcon.innerHTML = skill.icon;
              skillTitle.textContent = skill.title;
              skillDescription.textContent = skill.description;
              skillImage.src = skill.image;
              skillImage.alt = skill.title;
              skillContent.classList.add("animate-slide-fade");
            }, 100);
          });
        });
      |]
    ]

pricingScript :: Script
pricingScript = mconcat
  [ commonHeaderScrollScript
  , commonMobileMenuScript
  , commonSwiperScript
  , commonFadeInObserverScript
  , Script [istr|
      // Pricing: Pricing data for different regions
      const pricingData = {
        us: {
          junior: { price: "$5,000", savings: "Save up to 15%", savingsClass: "bg-gray-100 text-gray-600" },
          mid: { price: "$7,500", savings: "Save up to 50%", savingsClass: "bg-primary/10 text-primary" },
          senior: { price: "$10,000", savings: "Save up to 15%", savingsClass: "bg-gray-100 text-gray-600" },
        },
        eu: {
          junior: { price: "€4,200", savings: "Save up to 20%", savingsClass: "bg-green-100 text-green-600" },
          mid: { price: "€6,300", savings: "Save up to 55%", savingsClass: "bg-primary/10 text-primary" },
          senior: { price: "€8,400", savings: "Save up to 20%", savingsClass: "bg-green-100 text-green-600" },
        },
        asia: {
          junior: { price: "$3,500", savings: "Save up to 30%", savingsClass: "bg-blue-100 text-blue-600" },
          mid: { price: "₹4,50,000", savings: "Save up to 60%", savingsClass: "bg-primary/10 text-primary" },
          senior: { price: "$7,000", savings: "Save up to 30%", savingsClass: "bg-blue-100 text-blue-600" },
        },
        latam: {
          junior: { price: "$3,000", savings: "Save up to 40%", savingsClass: "bg-orange-100 text-orange-600" },
          mid: { price: "$4,500", savings: "Save up to 70%", savingsClass: "bg-primary/10 text-primary" },
          senior: { price: "$6,000", savings: "Save up to 40%", savingsClass: "bg-orange-100 text-orange-600" },
        },
        africa: {
          junior: { price: "$2,500", savings: "Save up to 50%", savingsClass: "bg-purple-100 text-purple-600" },
          mid: { price: "$3,750", savings: "Save up to 75%", savingsClass: "bg-primary/10 text-primary" },
          senior: { price: "$5,000", savings: "Save up to 50%", savingsClass: "bg-purple-100 text-purple-600" },
        },
      };

      // Pricing: Region selector functionality
      const regionSelect = document.getElementById("region-select");
      const pricingCards = document.getElementById("pricing-cards");
      function updatePricing(region) {
        const data = pricingData[region];
        if (!data) return;
        const cards = pricingCards.querySelectorAll(".pricing-card");
        regionSelect.classList.add("region-loading");
        setTimeout(() => {
          cards.forEach((card, index) => {
            const priceAmount = card.querySelector(".price-amount");
            const savingsBadge = card.querySelector(".savings-badge");
            priceAmount.classList.add("animate-price-change");
            savingsBadge.classList.add("animate-price-change");
            let cardData;
            if (index === 0) cardData = data.junior;
            else if (index === 1) cardData = data.mid;
            else cardData = data.senior;
            setTimeout(() => {
              priceAmount.textContent = cardData.price;
              savingsBadge.textContent = cardData.savings;
              savingsBadge.className = `savings-badge inline-block text-xs font-medium mt-5 px-2 py-1 rounded-full ${cardData.savingsClass}`;
              priceAmount.classList.remove("animate-price-change");
              savingsBadge.classList.remove("animate-price-change");
            }, 250);
          });
          regionSelect.classList.remove("region-loading");
        }, 300);
      }
      if (regionSelect) {
        regionSelect.addEventListener("change", function () {
          updatePricing(this.value);
        });
      }

      // Pricing: Pricing card button functionality
      const pricingButtons = document.querySelectorAll(".pricing-card button");
      pricingButtons.forEach((button, idx) => {
        button.addEventListener("click", function () {
          const card = this.closest(".pricing-card");
          const planTitle = card.querySelector("h3").textContent;
          const currentRegion = regionSelect.value;
          // Add your pricing plan selection logic here
        });
      });

      // Pricing: CTA button functionality
      const ctaButtons = document.querySelectorAll(".cta-card button");
      ctaButtons.forEach((button, idx) => {
        button.addEventListener("click", function () {
          const buttonText = this.textContent.trim();
          // Add your CTA logic here
        });
      });

      // Pricing: Hover effects for pricing cards
      document.querySelectorAll(".pricing-card").forEach((card, idx) => {
        card.addEventListener("mouseenter", function () {
          this.style.transform = "translateY(-5px)";
        });
        card.addEventListener("mouseleave", function () {
          this.style.transform = "translateY(0)";
        });
      });

      // Pricing: Hover effects for CTA cards
      document.querySelectorAll(".cta-card").forEach((card, idx) => {
        card.addEventListener("mouseenter", function () {
          this.style.transform = "translateY(-2px)";
        });
        card.addEventListener("mouseleave", function () {
          this.style.transform = "translateY(0)";
        });
      });

      // Pricing: Initialize with default region
      document.addEventListener("DOMContentLoaded", function () {
        updatePricing("us");
      });
    |]
  ]
