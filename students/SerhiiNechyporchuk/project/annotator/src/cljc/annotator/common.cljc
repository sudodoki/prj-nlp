(ns annotator.common
  (:require [clojure.string :as str]))

(defn shared-fn
  "A function that is shared between clj and cljs"
  []
  (println "cljc!"))

(defn job-progress [job]
  (let [filled (count (filter (fn [task] (some seq (:phrases task))) (:tasks job)))]
    (int (* 100 (/ filled (count (:tasks job)))))))

(def number-of-attributes [3 4 5])

(def values
  {:first-name     ["Altagracia" "Valarie" "Maya" "Lavern" "Chante" "Milly" "Erinn" "Stephenie" "Nancie" "Saran" "Emery" "Kate" "Tarah" "Vida" "Rosana" "Petra" "Carisa" "Vashti" "Marjorie" "Madonna"]
   :last-name      ["Smith" "Johnson" "Williams" "Jones" "Brown" "Davis" "Miller" "Wilson" "Moore" "Taylor" "Anderson" "Thomas" "Jackson" "White" "Harris" "Martin" "Thompson" "Garcia" "Martinez" "Robinson" "Clark" "Rodriguez" "Lewis" "Lee" "Walker"]
   :company        ["A. O. Smith" "A. Schulman" "A&W Restaurants" "a21, Inc." "Aaron's, Inc." "Abbott Laboratories" "AbbVie" "Abercrombie & Fitch" "Ablitech, Inc." "ABM Industries" "ABS Capital Partners" "ABX Air" "AC Lens" "Academi" "Accenture Plc" "Access Systems Americas, Inc." "ACCO Brands" "Accuquote" "Accuride Corporation" "Ace Hardware" "Acme Fresh Market" "ACN Inc." "Acsis Inc." "Activision Blizzard" "Activision" "Blizzard" "Acuity Brands" "Acuity Insurance"]
   :position       ["Accountant" "Accountant Systems" "Acquisition Management Intern" "Actuarial Analyst" "Actuary" "Administrative Generalist/Specialist" "Affordable Housing Specialist" "Analyst" "Appraiser" "Archaeologist" "Area Systems Coordinator" "Asylum or Immigration Officer" "Attorney/Law Clerk" "Audience Analyst" "Audit Resolution Follow Up" "Auditor" "Behavioral Scientist" "Biologist, Fishery" "Biologist, Marine" "Biologist, Wildlife" "Budget Analyst" "Budget Specialist" "Business Administration Officer" "Chemical Engineer" "Chemist" "Citizen Services Specialis"]
   :location       ["Afghanistan" "Albania" "Algeria" "Andorra" "Angola" "Antigua & Deps" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bhutan" "Bolivia" "Bosnia Herzegovina" "Botswana" "Brazil" "Brunei" "Bulgaria" "Burkina" "Burundi" "Cambodia" "Cameroon" "Canada" "Cape Verde" "Central African Rep" "Chad" "Chile" "China" "Colombia" "Comoros" "Congo" "Congo {Democratic Rep}" "Costa Rica" "Croatia" "Cuba" "Cyprus" "Czech Republic" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "East Timor" "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Eritrea" "Estonia" "Ethiopia" "Fiji" "Finland" "France" "Gabon" "Gambia" "Georgia" "Germany" "Ghana" "Greece" "Grenada" "Guatemala" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland {Republic}" "Israel" "Italy" "Ivory Coast" "Jamaica" "Japan" "Jordan" "Kazakhstan" "Kenya" "Kiribati" "Korea North" "Korea South" "Kosovo" "Kuwait" "Kyrgyzstan" "Laos" "Latvia" "Lebanon" "Lesotho" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Macedonia" "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta" "Marshall Islands" "Mauritania" "Mauritius" "Mexico" "Micronesia" "Moldova" "Monaco" "Mongolia" "Montenegro" "Morocco" "Mozambique" "Myanmar, {Burma}" "Namibia" "Nauru" "Nepal" "Netherlands" "New Zealand" "Nicaragua" "Niger" "Nigeria" "Norway" "Oman" "Pakistan" "Palau" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russian Federation" "Rwanda" "St Kitts & Nevis" "St Lucia" "Saint Vincent & the Grenadines" "Samoa" "San Marino" "Sao Tome & Principe" "Saudi Arabia" "Senegal" "Serbia" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Solomon Islands" "Somalia" "South Africa" "South Sudan" "Spain" "Sri Lanka" "Sudan" "Suriname" "Swaziland" "Sweden" "Switzerland" "Syria" "Taiwan" "Tajikistan" "Tanzania" "Thailand" "Togo" "Tonga" "Trinidad & Tobago" "Tunisia" "Turkey" "Turkmenistan" "Tuvalu" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom" "United States" "Uruguay" "Uzbekistan" "Vanuatu" "Vatican City" "Venezuela" "Vietnam" "Yemen" "Zambia" "Zimbabwe"]
   :enjoyment      ["enjoyed" "not enjoyed"]
   :sentiment      ["positive" "negative"]
   :email          ["mthurn@live.com" "fangorn@hotmail.com" "euice@outlook.com" "rgarcia@optonline.net" "mxiao@yahoo.com" "firstpr@att.net" "webdragon@comcast.net" "jguyer@aol.com" "sakusha@yahoo.ca" "crandall@sbcglobal.net" "drezet@me.com" "miyop@icloud.com"]
   :phone          ["(290) 448-8241" "(447) 865-0513" "(895) 592-8974" "(101) 892-2757" "(506) 934-8645" "(728) 690-5585" "(726) 292-2944" "(331) 408-2679" "(939) 771-3645" "(454) 726-1205" "(769) 916-5982" "(747) 180-7744" "(633) 561-7544" "(771) 171-3243" "(344) 842-7718"]
   :website        ["mapy.cz" "ow.ly" "bbb.org" "mayoclinic.com" "soundcloud.com" "free.fr" "stumbleupon.com" "hugedomains.com" "apache.org" "wikia.com" "privacy.gov.au" "bandcamp.com" "sphinn.com" "deviantart.com" "berkeley.edu"]
   :social-network ["google plus" "twitter" "google+" "facebook" "linkedin" "linked in"]
   :post-text      ["My creativity is funky, and I want to run a marathon. You've got sick encounters, forever. #elbowism #randomtweet" "My job is debt free, and I want to pluck a cactus. Ridiculously good dinner dates, again." "My cooking is being a jerk, and I want to go swimming. A hint of stellar banjo music, IMHO." "My life is a pain, and I want to be heard. We need clammy candy, for the love of God." "My boss is a joy, and I want to study holistic medicine. We need insane lies, really." "My cat is debt free, and I want to level up. A clear case of better glitter, please." "My job is so robust, and I want to go swimming. A hint of specific make-up, I say." "My honesty is a pain, and I want to play hacky sack. A path towards creepy costumes, again."]
   :post-word      ["mix" "finite" "phonograph" "fatal" "liver" "blizzard" "rare" "conqueror" "crawling" "accommodation" "gargoyle" "blackout" "coastal" "convulsion" "pragmatic" "beyond" "concussion" "free" "easy" "honey" "abomination" "eight" "captive" "big" "adsorbable" "airtight" "escapist" "feeling" "biplane"]
   :freq           (map str (range 10))
   :mention        ["Clay Arnold" "Sheryl Bradley" "Steven Owens" "Kristin Hansen" "Stephanie Jenkins" "Michelle Neal" "Lindsey Holt" "Philip Stephens" "Delia Flores" "Caleb Ramirez" "Lynn Boyd" "Andy Chandler" "Estelle Jimenez" "Stacy Vaughn" "Dorothy Larson" "Shawna Potter" "Sherman Cooper" "Hope Griffith" "Connie Mitchell" "Dolores Cummings"]
   :poll-answer    ["Yes" "No" "Maybe"]
   :poll-question  ["Do you sleep with your closet doors open?" "Do you take the shampoos and conditioner bottles from hotel?" "Do you sleep with your sheets tucked in?" "Have you ever stolen a street sign before?" "Do you like to use post-it notes?" "Do you cut out coupons but then never use them?" "Do you have freckles?" "Do you always smile for pictures?" "Do you ever count your steps when you walk?" "Have you ever peed in the woods?" "Do you ever dance even if there's no music playing?" "Do you chew your pens and pencils?" "Is it ok for guys to wear pink ?" "Do you still watch cartoons?"]
   :event          ["Fluffy Bison'17" "Tired Kangaroos'18" "Clear Nightingales'12" "Swanky Elks'01" "Tall Frogs'12" "Present Larks'05" "Luxuriant Apes'04" "Deeply Seals'15" "Many Worms'13" "Ill Guineapigs'10" "Vagabond Panthers'12" "Happy Hippopotamuss'17"]
   :page-type      ["session" "sponsor" "speaker" "exhibitor"]
   :file-name      ["talk.pdf" "presentation.pptx" "workshop.doc"]
   :stars          (map str (range 6))})

(def section->human
  {"profile" "has"
   "post" "wrote post"
   "like" "liked post"
   "comment" "replied to post"
   "poll" "participated in poll"
   "events" "attended event"
   "pageView" "viewed page"
   "rating" "rated page"
   "notes" "leave a note on page"
   "fileDownload" "downloaded a file"
   "ad" "clicked on ad"})

(def top-level #{"appSessions" "profile"})

(def search-attributes
  [{:name "profile.firstName" :short "has {first-name} as first name" :type "text" :example "Attendees with first name James"}
   {:name "profile.lastName" :short "has {last-name} as last name" :type "text" :example "Attendees with last name Bond"}
   {:name "profile.company" :short "is working at {company}" :type "text" :example "Attendees who worked at Google, *Employees from Google"}
   {:name "profile.position" :short "has {position} position" :type "text" :example "Attendees with CEO position, *Find all CEO"}
   {:name "profile.location" :short "has {location} as location" :type "text" :example "Attendees with Ukraine location, *Attendees from Ukraine"}
   {:name "profile.bio" :short "whose bio contains '{post-word}'" :type "text" :example "1"}
   {:name "profile.enjoyment" :short "have {enjoyment} the event" :type "Sentiment" :example "Find all attendees with high enjoyment, Attendees who enjoyed AI Summit event"}
   {:name "profile.email" :short "has {email} as email" :type "text" :example "Attendees with abc.com in email"}
   {:name "profile.phone" :short "has {phone} as phone" :type "text" :example "1"}
   {:name "profile.website" :short "has '{website}' as website" :type "text" :example "1"}
   {:name "profile.social_network" :short "has connected {social-network}" :type "bool" :example "Attendees with connected linkedin"}
   {:name "post.text" :short "contains '{post-word}'" :human "wrote post '{post-text}'" :type "text" :example "Attendees who wrote post with Tesla, *Attendees who wrote post about Tesla"}
   {:name "post.count" :short "{freq} times" :human "wrote {freq} posts" :type "int" :example "Attendees who more than 10 posts"}
   {:name "post.likesCount" :short "with {freq} likes " :human "wrote post with {freq} likes" :type "int" :example "Attenddes who create post with more than 10 likes"}
   {:name "post.mentions" :short "with {mention}'s mention " :human "mentioned {mention} in a post" :type "Attendee" :example "Attendees who mentioned Elon Musk in post"}
   {:name "post.commentsCount" :short "with {freq} replies" :human "wrote post with {freq} replies" :type "int" :example "Attendees who create post with more than 10 comments"}
   {:name "post.sentiment" :short "with {sentiment} sentiment" :human "wrote {sentiment} post" :type "Sentiment" :example "Attendees who wrote positive post with Tesla"}
   {:name "like.postText" :short "contains '{post-word}'" :human "liked post '{post-text}'" :type "text" :example "Attendees who liked post with Tesla"}
   {:name "like.postAuthor" :short "by {mention}" :human "liked {mention}'s post" :type "Attendee" :example "Attendees who liked posts by Elon Musk"}
   {:name "like.postSentiment" :short "with {sentiment} sentiment" :human "liked {sentiment} post" :type "Sentiment" :example "Attendees who liked positive post with Tesla"}
   {:name "like.count" :short "{freq} times" :human "made {freq} likes" :type "int" :example "Attendees who made more than 30 likes"}
   {:name "comment.text" :short "with '{post-word}'" :human "wrote reply '{post-text}'" :type "text" :example "Attendees who wrote comment with Tesla"}
   {:name "comment.postText" :short "<em>(post)</em> contains '{post-word}'" :human "wrote reply to the post '{post-text}'" :type "text" :example "Attendees who wrote comment with Tesla for the post with SpaceX"}
   {:name "comment.postAuthor" :short "by {mention}" :human "wrote reply to {mention}'s post" :type "Attendee" :example "Attenddes who wrote comment for Elon Musk post"}
   {:name "comment.postSentiment" :short "<em>(post)</em> with {sentiment} sentiment" :human "wrote reply to {sentiment} post" :type "Sentiment" :example "Attendees who wrote comment for negative post with Boring"}
   {:name "poll.questionText" :short "with question '{poll-question}'" :type "text" :example "Attendees who answered for poll 'Are you happy?'"}
   {:name "poll.answerText" :short "answered '{poll-answer}'" :type "text" :example "Attendees who answered Yes for poll 'Are you happy?'"}
   {:name "poll.count" :short "{freq} times"}
   {:name "appSessions.count" :short "used app {freq} times" :type "int" :example "Attendees with less than 10 app sessions"}
   {:name "events.name" :short "'{event}'"}
   {:name "events.count" :short "{freq} times"}
   {:name "pageView.content" :short "with '{post-word}'"  :type "text" :example "Attendees who viewed page with SpaceX"}
   {:name "pageView.type" :short "with '{page-type}' type" :type "PageType" :example "Attendees who viewed sponsor page"}
   {:name "pageView.count" :short "{freq} times" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
   {:name "rating.content" :short "contains '{post-word}'"  :type "text" :example "Attendees who viewed page with SpaceX"}
   {:name "rating.type" :short "with '{page-type}' type" :type "PageType" :example "Attendees who viewed sponsor page"}
   {:name "rating.count" :short "{freq} times" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
   {:name "rating.value" :short "with {stars} stars"}
   {:name "notes.content" :short "contains '{post-word}'"  :type "text" :example "Attendees who viewed page with SpaceX"}
   {:name "notes.type" :short "with '{page-type}' type" :type "PageType" :example "Attendees who viewed sponsor page"}
   {:name "notes.count" :short "{freq} times" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
   {:name "fileDownload.content" :short "from page contains '{post-word}'"  :type "text" :example "Attendees who viewed page with SpaceX"}
   {:name "fileDownload.type" :short "from page with '{page-type}' type" :type "PageType" :example "Attendees who viewed sponsor page"}
   {:name "fileDownload.count" :short "{freq} times" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
   {:name "fileDownload.name" :short "with name '{file-name}'" :type "int" :example "Attendees who viewed sponsor page with Tesla more than 5 times"}
   {:name "ad.text" :short "contains '{post-word}'"  :type "text" :example "Attendees who clicked on ad with Tesla"}
   {:name "ad.sponsor" :short "by '{company}'" :type "Sponsor" :example "Attendees who clicked Apply ad with 'Wanna new iPhone?'"}
   {:name "ad.count" :short "{freq} times" :type "int" :example "Attendees who clicked on ad more than 2 times"}]
  )

