(ns annotator.common
  (:require [clojure.string :as str]))

(defn shared-fn
  "A function that is shared between clj and cljs"
  []
  (println "cljc!"))

(def number-of-attributes [3 4 5])

(def values
  {:first-name     ["Altagracia" "Valarie" "Maya" "Lavern" "Chante" "Milly" "Erinn" "Stephenie" "Nancie" "Saran" "Emery" "Kate" "Tarah" "Vida" "Rosana" "Petra" "Carisa" "Vashti" "Marjorie" "Madonna"]
   :last-name      ["Smith" "Johnson" "Williams" "Jones" "Brown" "Davis" "Miller" "Wilson" "Moore" "Taylor" "Anderson" "Thomas" "Jackson" "White" "Harris" "Martin" "Thompson" "Garcia" "Martinez" "Robinson" "Clark" "Rodriguez" "Lewis" "Lee" "Walker"]
   :company        ["A. O. Smith" "A. Schulman" "A&W Restaurants" "a21, Inc." "Aaron's, Inc." "Abbott Laboratories" "AbbVie" "Abercrombie & Fitch" "Ablitech, Inc." "ABM Industries" "ABS Capital Partners" "ABX Air" "AC Lens" "Academi" "Accenture Plc" "Access Systems Americas, Inc." "ACCO Brands" "Accuquote" "Accuride Corporation" "Ace Hardware" "Acme Fresh Market" "ACN Inc." "Acsis Inc." "Activision Blizzard" "Activision" "Blizzard" "Acuity Brands" "Acuity Insurance"]
   :position       ["Accountant" "Accountant Systems" "Acquisition Management Intern" "Actuarial Analyst" "Actuary" "Administrative Generalist/Specialist" "Affordable Housing Specialist" "Analyst" "Appraiser" "Archaeologist" "Area Systems Coordinator" "Asylum or Immigration Officer" "Attorney/Law Clerk" "Audience Analyst" "Audit Resolution Follow Up" "Auditor" "Behavioral Scientist" "Biologist, Fishery" "Biologist, Marine" "Biologist, Wildlife" "Budget Analyst" "Budget Specialist" "Business Administration Officer" "Chemical Engineer" "Chemist" "Citizen Services Specialis"]
   :location       ["Afghanistan" "Albania" "Algeria" "Andorra" "Angola" "Antigua & Deps" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bhutan" "Bolivia" "Bosnia Herzegovina" "Botswana" "Brazil" "Brunei" "Bulgaria" "Burkina" "Burundi" "Cambodia" "Cameroon" "Canada" "Cape Verde" "Central African Rep" "Chad" "Chile" "China" "Colombia" "Comoros" "Congo" "Congo {Democratic Rep}" "Costa Rica" "Croatia" "Cuba" "Cyprus" "Czech Republic" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "East Timor" "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Eritrea" "Estonia" "Ethiopia" "Fiji" "Finland" "France" "Gabon" "Gambia" "Georgia" "Germany" "Ghana" "Greece" "Grenada" "Guatemala" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland {Republic}" "Israel" "Italy" "Ivory Coast" "Jamaica" "Japan" "Jordan" "Kazakhstan" "Kenya" "Kiribati" "Korea North" "Korea South" "Kosovo" "Kuwait" "Kyrgyzstan" "Laos" "Latvia" "Lebanon" "Lesotho" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Macedonia" "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta" "Marshall Islands" "Mauritania" "Mauritius" "Mexico" "Micronesia" "Moldova" "Monaco" "Mongolia" "Montenegro" "Morocco" "Mozambique" "Myanmar, {Burma}" "Namibia" "Nauru" "Nepal" "Netherlands" "New Zealand" "Nicaragua" "Niger" "Nigeria" "Norway" "Oman" "Pakistan" "Palau" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russian Federation" "Rwanda" "St Kitts & Nevis" "St Lucia" "Saint Vincent & the Grenadines" "Samoa" "San Marino" "Sao Tome & Principe" "Saudi Arabia" "Senegal" "Serbia" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Solomon Islands" "Somalia" "South Africa" "South Sudan" "Spain" "Sri Lanka" "Sudan" "Suriname" "Swaziland" "Sweden" "Switzerland" "Syria" "Taiwan" "Tajikistan" "Tanzania" "Thailand" "Togo" "Tonga" "Trinidad & Tobago" "Tunisia" "Turkey" "Turkmenistan" "Tuvalu" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom" "United States" "Uruguay" "Uzbekistan" "Vanuatu" "Vatican City" "Venezuela" "Vietnam" "Yemen" "Zambia" "Zimbabwe"]
   #_#_:bio            []
   :enjoyment      ["enjoyed" "not enjoyed"]
   :sentiment      ["positive" "negative"]
   :email          ["mthurn@live.com" "fangorn@hotmail.com" "euice@outlook.com" "rgarcia@optonline.net" "mxiao@yahoo.com" "firstpr@att.net" "webdragon@comcast.net" "jguyer@aol.com" "sakusha@yahoo.ca" "crandall@sbcglobal.net" "drezet@me.com" "miyop@icloud.com"]
   :phone          ["(290) 448-8241" "(447) 865-0513" "(895) 592-8974" "(101) 892-2757" "(506) 934-8645" "(728) 690-5585" "(726) 292-2944" "(331) 408-2679" "(939) 771-3645" "(454) 726-1205" "(769) 916-5982" "(747) 180-7744" "(633) 561-7544" "(771) 171-3243" "(344) 842-7718"]
   :website        ["mapy.cz" "ow.ly" "bbb.org" "mayoclinic.com" "soundcloud.com" "free.fr" "stumbleupon.com" "hugedomains.com" "apache.org" "wikia.com" "privacy.gov.au" "bandcamp.com" "sphinn.com" "deviantart.com" "berkeley.edu"]
   :social-network ["google plus" "twitter" "google+" "facebook" "linkedin" "linked in"]
   :post-text      ["My creativity is funky, and I want to run a marathon. You've got sick encounters, forever. #elbowism #randomtweet" "My job is debt free, and I want to pluck a cactus. Ridiculously good dinner dates, again." "My cooking is being a jerk, and I want to go swimming. A hint of stellar banjo music, IMHO." "My life is a pain, and I want to be heard. We need clammy candy, for the love of God." "My boss is a joy, and I want to study holistic medicine. We need insane lies, really." "My cat is debt free, and I want to level up. A clear case of better glitter, please." "My job is so robust, and I want to go swimming. A hint of specific make-up, I say." "My honesty is a pain, and I want to play hacky sack. A path towards creepy costumes, again."]
   :freq           (map str (range 10))
   :mention        ["Clay Arnold" "Sheryl Bradley" "Steven Owens" "Kristin Hansen" "Stephanie Jenkins" "Michelle Neal" "Lindsey Holt" "Philip Stephens" "Delia Flores" "Caleb Ramirez" "Lynn Boyd" "Andy Chandler" "Estelle Jimenez" "Stacy Vaughn" "Dorothy Larson" "Shawna Potter" "Sherman Cooper" "Hope Griffith" "Connie Mitchell" "Dolores Cummings"]})

(def search-attributes
  [{:name "profile.firstName" :human "has {first-name} as first name" :type "text" :example "Attendees with first name James"}
   {:name "profile.lastName" :human "has {last-name} as last name" :type "text" :example "Attendees with last name Bond"}
   {:name "profile.company" :human "is working at {company}" :type "text" :example "Attendees who worked at Google, *Employees from Google"}
   {:name "profile.position" :human "has {position} position" :type "text" :example "Attendees with CEO position, *Find all CEO"}
   {:name "profile.location" :human "has {location} as location" :type "text" :example "Attendees with Ukraine location, *Attendees from Ukraine"}
   #_{:name "profile.bio" :human "Attendee's bio is '{bio}'" :type "text" :example "1"}
   {:name "profile.enjoyment" :human "have {enjoyment} the event" :type "Sentiment" :example "Find all attendees with high enjoyment, Attendees who enjoyed AI Summit event"}
   {:name "profile.email" :human "has {email} as email" :type "text" :example "Attendees with abc.com in email"}
   {:name "profile.phone" :human "has {phone} as phone" :type "text" :example "1"}
   {:name "profile.website" :human "has '{website}' as website" :type "text" :example "1"}
   {:name "profile.social_network" :human "has connected {social-network}" :type "bool" :example "Attendees with connected linkedin"}
   {:name "post.text" :human "wrote post '{post-text}'" :type "text" :example "Attendees who wrote post with Tesla, *Attendees who wrote post about Tesla"}
   {:name "post.count" :human "wrote {freq} posts" :type "int" :example "Attendees who more than 10 posts"}
   {:name "post.likesCount" :human "wrote post with {freq} likes" :type "int" :example "Attenddes who create post with more than 10 likes"}
   {:name "post.mentions" :human "mentioned {mention} in a post" :type "Attendee" :example "Attendees who mentioned Elon Musk in post"}
   {:name "post.commentsCount" :human "wrote post with {freq} replies" :type "int" :example "Attendees who create post with more than 10 comments"}
   {:name "post.sentiment" :human "wrote {sentiment} post" :type "Sentiment" :example "Attendees who wrote positive post with Tesla"}
   {:name "like.postText" :human "liked post '{post-text}'" :type "text" :example "Attendees who liked post with Tesla"}
   {:name "like.postAuthor" :human "liked {mention}'s post" :type "Attendee" :example "Attendees who liked posts by Elon Musk"}
   {:name "like.postSentiment" :human "liked {sentiment} post" :type "Sentiment" :example "Attendees who liked positive post with Tesla"}
   {:name "like.count" :human "made {freq} likes" :type "int" :example "Attendees who made more than 30 likes"}
   {:name "comment.text" :human "wrote reply '{post-text}'" :type "text" :example "Attendees who wrote comment with Tesla"}
   {:name "comment.postText" :human "wrote reply to the post '{post-text}'" :type "text" :example "Attendees who wrote comment with Tesla for the post with SpaceX"}
   {:name "comment.postAuthor" :human "wrote reply to {mention}'s post" :type "Attendee" :example "Attenddes who wrote comment for Elon Musk post"}
   {:name "comment.postSentiment" :human "wrote reply to {sentiment} post" :type "Sentiment" :example "Attendees who wrote comment for negative post with Boring"}
   #_#_#_#_#_#_#_#_#_
       {:name "poll.questionText" :type "text" :example "Attendees who answered for poll 'Are you happy?'"}
       {:name "poll.answerText" :type "text" :example "Attendees who answered Yes for poll 'Are you happy?'"}
       {:name "appSessions.count" :type "int" :example "Attendees with less than 10 app sessions"}
       {:name "pageView.content" :type "text" :example "Attendees who viewed page with SpaceX"}
       {:name "pageView.type" :type "PageType" :example "Attendees who viewed sponsor page"}
       {:name "pageView.count" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
       {:name "ad.text" :type "text" :example "Attendees who clicked on ad with Tesla"}
       {:name "ad.sponsor" :type "Sponsor" :example "Attendees who clicked Apply ad with 'Wanna new iPhone?'"}
       {:name "ad.count" :type "int" :example "Attendees who clicked on ad more than 2 times"}]
  )

