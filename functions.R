
# Generator functions -----------------------------------------------------


# Helpers -----------------------------------------------------------------

# qsample() quickly samples 1 item from any vector (saves typing)
qsample <- function(x) {
  sample(x, 1)
}



# Region name -------------------------------------------------------------

name_region <- function() {
  
  terrain <- c("Bay", "Bluffs", "Bog", "Cliffs", "Desert", "Downs", "Dunes", "Expanse", "Fells", "Fen", "Flats", "Foothills", "Forest", "Groves", "Heath", "Heights", "Hills", "Hollows", "Jungle", "Lake", "Lowland", "March", "Marsh", "Meadows", "Moor", "Morass", "Mounds", "Mountains", "Peaks", "Plains", "Prairie", "Quagmire", "Range", "Reach", "Sands", "Savanna", "Scarps", "Sea", "Slough", "Sound", "Steppe", "Swamp", "Sweep", "Teeth", "Thicket", "Upland", "Wall", "Waste", "Wasteland", "Woods"
  )
  
  adjective <- c("Ageless", "Ashen", "Black", "Blessed", "Blighted", "Blue", "Broken", "Burning", "Cold", "Cursed", "Dark", "Dead", "Deadly", "Deep", "Desolate", "Diamond", "Dim", "Dismal", "Dun", "Eerie", "Endless", "Fallen", "Far", "Fell", "Flaming", "Forgotten", "Forsaken", "Frozen", "Glittering", "Golden", "Green", "Grim", "Holy", "Impassable", "Jagged", "Light", "Long", "Misty", "Perilous", "Purple", "Red", "Savage", "Shadowy", "Shattered", "Shifting", "Shining", "Silver", "White", "Wicked", "Yellow")
  
  noun <- c("[Name]*", "Ash", "Bone", "Darkness", "Dead", "Death", "Desolation", "Despair", "Devil", "Doom", "Dragon", "Fate", "Fear", "Fire", "Fury", "Ghost", "Giant", "God", "Gold", "Heaven", "Hell", "Honor", "Hope", "Horror", "King", "Life", "Light", "Lord", "Mist", "Peril", "Queen", "Rain", "Refuge", "Regret", "Savior", "Shadow", "Silver", "Skull", "Sky", "Smoke", "Snake", "Sorrow", "Storm", "Sun", "Thorn", "Thunder", "Traitor", "Troll", "Victory", "Witch")
  
  names_list <- data.frame(terrain, adjective, noun, stringsAsFactors = FALSE)
  roll <- sample(1:12, 1)
  
  if (roll %in% c(1:4)) {
    region_name <- paste(
      qsample(names_list$adjective), 
      qsample(names_list$terrain)
    )
  }
  
  if (roll %in% c(5:6)) {
    region_name <- paste(
      qsample(names_list$terrain),
      "of (the)", 
      qsample(names_list$noun)
    )
  }
  
  if (roll %in% c(7:8)) {
    region_name <- paste(
      "The",
      qsample(names_list$terrain),
      "of (the)", 
      qsample(names_list$adjective)
    )
  }
  
  if (roll %in% c(9:10)) {
    region_name <- paste(
      qsample(names_list$noun),
      qsample(names_list$terrain)
    )
  }
  
  if (roll == 11) {
    region_name <- paste0(
      "The ",
      qsample(names_list$noun),
      "'s ",
      qsample(names_list$adjective),
      " ",
      qsample(names_list$terrain)
    )
  }
  
  if (roll == 12) {
    region_name <- paste0(
      qsample(names_list$adjective),
      " ",
      qsample(names_list$terrain),
      " of (the) ",
      qsample(names_list$noun)
    )
  }
  
  return(region_name)
}


# Place name --------------------------------------------------------------

name_place <- function() {
  
}


# Subtables ---------------------------------------------------------------

roll_details <- function(roll_table = "Oddity") {
  
  # acceptable inputs
  tables_available <- c(
    "Ability",
    "Activity",
    "Adjective",
    "Age",
    "Alignment",
    "Aspect",
    "Condition",
    "Disposition",
    "Element",
    "Magic Type",
    "No. Appearing",
    "Oddity",
    "Feature",
    "Orientation",
    "Ruination",
    "Size",
    "Tag",
    "Terrain",
    "Visibility"
  )
  
  # input validation
  if (!roll_table %in% tables_available) {
    stop(paste("invalid roll_table:", roll_table))
  }
  
  if (roll_table == "Ability") {
    result <-  qsample(
      c(
        "bless/curse",
        "entangle/trap/snare",
        "poison/disease",
        "paralyze/petrify",
        "mimic/camouflage",
        "seduce/hypnotize",
        "dissolve/disintegrate",
        qsample(
          c(
            "divination",
            "enchantment",
            "evocation",
            "illusion",
            "necromancy",
            "summoning"
          )
        ),
        "drain life/drain magic",
        paste("immunity:", 
              qsample(
                c(
                  "air",
                  "earth",
                  "fire",
                  "water",
                  "life",
                  "death"
                )
              ), collapse = ""),
        "read/control minds",
        paste(sample(c("bless/curse",
                       "entangle/trap/snare",
                       "poison/disease",
                       "paralyze/petrify",
                       "mimic/camouflage",
                       "seduce/hypnotize",
                       "dissolve/disintegrate",
                       qsample(
                         c(
                           "divination",
                           "enchantment",
                           "evocation",
                           "illusion",
                           "necromancy",
                           "summoning"
                         )
                       ),
                       "drain life/drain magic",
                       paste("immunity:", 
                             qsample(
                               c(
                                 "air",
                                 "earth",
                                 "fire",
                                 "water",
                                 "life",
                                 "death"
                               )
                             )),
                       "read/control minds"), 2), collapse = ", ")
      )
    )
    
    
  }
  
  if (roll_table == "Activity") {
    result <- qsample(
      c(
        "laying a trap/ambush",
        "fighting/at war",
        "prowling/on patrol",
        "hunting/foraging",
        "eating/resting",
        "crafting/praying",
        "traveling/relocating",
        "exploring/lost",
        "returning home",
        "building/excavating",
        "sleeping",
        "dying"
      )
    )
  }
  
  if (roll_table == "Adjective") {
    result <- qsample(
      c(
        "slick/slimy",
        "rough/hard/sharp",
        "smooth/soft/dull",
        "corroded/rusty",
        "rotten/decaying",
        "broken/brittle",
        "stinking/smelly",
        "weak/thin/drained",
        "strong/fat/full",
        "pale/poor/shallow",
        "dark/rich/deep",
        "colorful"
      )
    )
  }
  
  if (roll_table == "Age") {
    result <- qsample(
      c(
        "being born/built",
        rep("young/recent", 3),
        rep("middle-aged", 3),
        rep("old", 2),
        rep("ancient", 2),
        "pre-historic"
      )
    )
  }
  
  if (roll_table == "Alignment") {
    result <- qsample(
      c(
        rep("Chaotic", 2),
        rep("Evil", 4),
        rep("Neutral", 4),
        rep("Good", 1),
        rep("Lawful", 2)
      )
    )
  }
  
  if (roll_table == "Aspect") {
    result <- qsample(
      c(
        "power/strength",
        "trickery/dexterity",
        "time/constitution",
        "knowledge/intelligence",
        "nature/wisdom",
        "culture/charisma",
        "war/lies/discord",
        "peace/truth/balance",
        "hate/envy",
        "love/admiration",
        qsample(
          c(
            "air",
            "earth",
            "fire",
            "water",
            "life",
            "death"
          )
        ),
        paste(
          qsample(
            c(
              "power/strength",
              "trickery/dexterity",
              "time/constitution",
              "knowledge/intelligence",
              "nature/wisdom",
              "culture/charisma",
              "war/lies/discord",
              "peace/truth/balance",
              "hate/envy",
              "love/admiration",
              qsample(
                c(
                  "air",
                  "earth",
                  "fire",
                  "water",
                  "life",
                  "death"
                )
              )
            )
          ),
          qsample(
            c(
              "power/strength",
              "trickery/dexterity",
              "time/constitution",
              "knowledge/intelligence",
              "nature/wisdom",
              "culture/charisma",
              "war/lies/discord",
              "peace/truth/balance",
              "hate/envy",
              "love/admiration",
              qsample(
                c(
                  "air",
                  "earth",
                  "fire",
                  "water",
                  "life",
                  "death"
                )
              )
            )
          )
          
          , sep = ", ")
      )
    )
  }
  
  if (roll_table == "Condition") {
    result <- qsample(
      c(
        "being built/born",
        rep("intact/healthy/stable", 3),
        rep("occupied/active/alert", 3),
        rep("worn/tired/weak", 2),
        "vacant/lost",
        "ruined/defiled/dying",
        "disappeared/dead"
      )
    )
  }
  
  if (roll_table == "Disposition") {
    result <- qsample(
      c(
        "attacking",
        rep("hostile/aggressive", 3),
        rep("cautious/doubtful", 2),
        "fearful/fleeing",
        rep("neutral", 2),
        "curious/hopeful",
        "friendly"
      )
    )
  }
  
  if (roll_table == "Element") {
    result <- qsample(
      c(
        "air",
        "earth",
        "fire",
        "water",
        "life",
        "death"
      )
    )
  }
  
  if (roll_table == "Magic Type") {
    result <- qsample(
      c(
        "divination",
        "enchantment",
        "evocation",
        "illusion",
        "necromancy",
        "summoning"
      )
    )
  }
  
  if (roll_table == "No. Appearing") {
    result <- qsample(
      c(
        rep("Solitary (1)", 5),
        rep(paste0("Group (", sample(1:6, 1)+2, ")" ), 5),
        rep(paste0("Horde (", sum(sample(1:6, 4)), " per wave)" ), 2)
      )
    )
  }
  
  if (roll_table == "Oddity") {
    result <- qsample(
      c(
        "weird color/smell/sound",
        "geometric",
        "web/network/system",
        "crystalline/glass-like ",
        "fungal",
        "gaseous/smokey",
        "mirage/illusion",
        "volcanic/explosive",
        "magnetic/repellant",
        "devoid of life",
        "unexpectedly alive",
        paste(sample(c(
          "weird color/smell/sound",
          "geometric",
          "web/network/system",
          "crystalline/glass-like ",
          "fungal",
          "gaseous/smokey",
          "mirage/illusion",
          "volcanic/explosive",
          "magnetic/repellant",
          "devoid of life",
          "unexpectedly alive"), 2), collapse = ", ")
      )
    )
  }
  
  if (roll_table == "Feature") {
    roll_die <- sample(1:12, 1)
    
    if (roll_die < 8) {
      result <- qsample(c("heavily armored", 
                          "winged", 
                          "flying", 
                          "multiple heads/headless", 
                          "many eyes/one eye", 
                          "many limbs/tails", 
                          "tentacles/tendrils"))
    }
    if (roll_die == 12) {
      result <- paste(
        sample(c("heavily armored", 
                 "winged", 
                 "flying", 
                 "multiple heads/headless", 
                 "many eyes/one eye", 
                 "many limbs/tails", 
                 "tentacles/tendrils"), 2)
        , collapse = ", ")
    }
    
    if (roll_die == 8) {
      result <- qsample(
        c(
          "power/strength",
          "trickery/dexterity",
          "time/constitution",
          "knowledge/intelligence",
          "nature/wisdom",
          "culture/charisma",
          "war/lies/discord",
          "peace/truth/balance",
          "hate/envy",
          "love/admiration",
          qsample(
            c(
              "air",
              "earth",
              "fire",
              "water",
              "life",
              "death"
            )
          ),
          paste(
            qsample(
              c(
                "power/strength",
                "trickery/dexterity",
                "time/constitution",
                "knowledge/intelligence",
                "nature/wisdom",
                "culture/charisma",
                "war/lies/discord",
                "peace/truth/balance",
                "hate/envy",
                "love/admiration",
                qsample(
                  c(
                    "air",
                    "earth",
                    "fire",
                    "water",
                    "life",
                    "death"
                  )
                )
              )
            ),
            qsample(
              c(
                "power/strength",
                "trickery/dexterity",
                "time/constitution",
                "knowledge/intelligence",
                "nature/wisdom",
                "culture/charisma",
                "war/lies/discord",
                "peace/truth/balance",
                "hate/envy",
                "love/admiration",
                qsample(
                  c(
                    "air",
                    "earth",
                    "fire",
                    "water",
                    "life",
                    "death"
                  )
                )
              )
            )
            
            , sep = ", ")
        )
      )
    }
    
    if (roll_die == 9) {
      result <- qsample(
        c(
          "air",
          "earth",
          "fire",
          "water",
          "life",
          "death"
        )
      )
    }
    
    if (roll_die == 10) {
      result <- qsample(
        c(
          "divination",
          "enchantment",
          "evocation",
          "illusion",
          "necromancy",
          "summoning"
        )
      )
    }
    
    if (roll_die == 11) {
      result <- qsample(
        c(
          "weird color/smell/sound",
          "geometric",
          "web/network/system",
          "crystalline/glass-like ",
          "fungal",
          "gaseous/smokey",
          "mirage/illusion",
          "volcanic/explosive",
          "magnetic/repellant",
          "devoid of life",
          "unexpectedly alive",
          paste(sample(c(
            "weird color/smell/sound",
            "geometric",
            "web/network/system",
            "crystalline/glass-like ",
            "fungal",
            "gaseous/smokey",
            "mirage/illusion",
            "volcanic/explosive",
            "magnetic/repellant",
            "devoid of life",
            "unexpectedly alive"), 2), collapse = ", ")
        )
      )
    }
  }
  
  if (roll_table == "Orientation") {
    result <- qsample(
      c(
        "down/earthward",
        "down",
        "north",
        "northeast",
        "east",
        "southeast",
        "south",
        "southwest",
        "west",
        "northwest",
        "up",
        "up/skywards"
      )
    )
  }
  
  if (roll_table == "Ruination") {
    result <- qsample(
      c(
        "arcane disaster",
        "damnations/curse",
        rep("earthquake/fire/flood", 2),
        rep("plague/famine/drought", 2),
        rep("overrun by monsters", 2),
        rep("war/invasion", 2),
        "depleted resources",
        "better prospects elsewhere"
      )
    )
  }
  
  if (roll_table == "Size") {
    result <- qsample(
      c(
        "tiny",
        rep("small", 2),
        rep("medium-sized", 6),
        rep("large", 2),
        "huge"
      )
    )
  }
  
  if (roll_table == "Tag") {
    result <- qsample(
      c(
        "Amorphous",
        "Cautious",
        "Construct",
        "Devious",
        "Intelligent",
        "Magical",
        "Organized",
        "Organized",
        "Planar",
        "Stealthy",
        "Terrifying",
        paste(
          sample(
            c("Amorphous",
              "Cautious",
              "Construct",
              "Devious",
              "Intelligent",
              "Magical",
              "Organized",
              "Planar",
              "Stealthy",
              "Terrifying"), 2
          )
          , collapse = ", ")
      )
    )
  }
  
  if (roll_table == "Terrain") {
    result <- qsample(
      c(
        "wasteland/desert",
        "flatland/plain",
        "flatland/plain",
        "wetlands/marsh/swamp",
        "woodlands/forest/jungle",
        "woodlands/forest/jungle",
        "woodlands/forest/jungle",
        "highlands/hills",
        "highlands/hills",
        "mountains",
        "mountains",
        qsample(
          c(
            "weird color/smell/sound",
            "geometric",
            "web/network/system",
            "crystalline/glass-like ",
            "fungal",
            "gaseous/smokey",
            "mirage/illusion",
            "volcanic/explosive",
            "magnetic/repellant",
            "devoid of life",
            "unexpectedly alive",
            paste(sample(c(
              "weird color/smell/sound",
              "geometric",
              "web/network/system",
              "crystalline/glass-like ",
              "fungal",
              "gaseous/smokey",
              "mirage/illusion",
              "volcanic/explosive",
              "magnetic/repellant",
              "devoid of life",
              "unexpectedly alive"), 2), collapse = ", ")
          )
        )
      )
    )
  }
  
  if (roll_table == "Visibility") {
    result <- qsample(
      c(
        rep("buried/camouflaged/nigh invisible", 2),
        rep("partly covered/overgrown/hidden", 4),
        rep("obvious/in plain sight", 3),
        rep("visible at near distance", 2),
        "visible at great distance/focal point"
      )
    )
  }
  
  if (!exists("result")) {
    return("No valid type requested")
  }
  return(result)
  
}

roll_utility_item <- function() {
  item <- c(
    "key/lockpick",
    "potion/food",
    "clothing/cloak",
    "decanter/vessel/cup",
    "cage/box/coffer",
    "instrument/toolbook/scroll",
    "weapon/staff/wand",
    "armor/shield/helm",
    "mirror/hourglass",
    "pet/mount",
    "device/construct"
  )
  
  return(qsample(item))
}

roll_art_item <- function() {
  item <- c(
    "trinket/charm",
    "painting/pottery",
    "ring/gloves",
    "carpet/tapestry",
    "statuette/idol",
    "flag/banner",
    "bracelet/armband",
    "necklace/amulet",
    "belt/harness",
    "hat/mask",
    "orb/sigil/rod",
    "crown/scepter"
  )
  
  return(qsample(item))
}

roll_treasure <- function(monster = "no") {
  
  if (class(monster) != "data.frame") {
    loot_roll <- sample(1:6, 2, replace = TRUE)
    if (loot_roll[1] == 6 & loot_roll[2] == 6) {
      loot_roll <- sum(sample(1:6, 3, replace = TRUE))
    }
    loot_roll <- sum(min(loot_roll))
    
  }else{
    
    if ("damage_die" %in% names(monster)) {
      loot_roll <- sample(1:monster$damage_die, 1)
    }
    if ("noteworthy" %in% names(monster)) {
      if (monster$noteworthy == TRUE) {
        loot_roll <- loot_roll + sample(1:4, 1)
      }
    }
    
  }
  
  loot <- c(
    paste(sum(sample(1:8, 2, replace = TRUE)), "coins"),
    "a useful item",
    paste(sum(sample(1:10, 4, replace = TRUE)), "coins"),
    paste("A small", roll_art_item(), "worth about", sum(sample(2:10, 2, replace = TRUE))*10, "coins"),
    paste("A", ifelse(sum(sample(1:6, 2, replace = TRUE)) > 8, 
                      roll_utility_item(), 
                      roll_art_item()), 
          "imbued with minor powers of", 
          ifelse(sample(1:2,1) == 1, 
                 roll_details("Magic Type"), 
                 roll_details("Ability"))),
    "a useful clue (map, note, etc)",
    paste("A bag of", sample(1:4,1, replace = TRUE)*100 ,"coins (1 weight per 100)"),
    paste0("A small ", roll_art_item(), " of great value (", sum(sample(1:6, 2, replace = TRUE))*100, " coins, 0 weight)"),
    paste("A chest of coins and other small valuables.", paste0("(", sum(sample(1:6, 3, replace = TRUE)) * 100), "coins, 1 weight)"),
    paste("A", ifelse(sum(sample(1:6, 2, replace = TRUE)) > 8, 
                      roll_utility_item(), 
                      roll_art_item()), 
          "imbued with the power of", 
          ifelse(sample(1:2, 1, replace = TRUE) == 1, 
                 roll_details("Magic Type"), 
                 roll_details("Ability"))),
    paste("Many small bags of coins,", sum(sample(1:4, 2, replace = TRUE))*100, "coins or so"),
    paste(paste0("Crown/banner of an ", roll_npc_occupation("Official")), "worth at least", sum(sample(1:4,3))*100, "coins"),
    paste("A large and ornate", roll_art_item(), paste0("(", sum(sample(1:4, 4, replace = TRUE))*100, " coins, 1 weight)")),
    paste("A unique", ifelse(sum(sample(1:6, 2, replace = TRUE)) > 8, 
                             roll_utility_item(), 
                             roll_art_item()), "worth at least", sum(sample(1:4, 5, replace = TRUE))*100, "coins"))
  
  loot_and_reroll <- c(
    paste("Everything you need to learn a new spell, and", tolower(qsample(loot))),
    paste("A portal or a secret path (or directions to one), and", tolower(qsample(loot))),
    paste("Something relating to one of the characters, and", tolower(qsample(loot))),
    paste("A hoard:", sample(1:10,1)*1000, 
          "coins, and", sample(1:10,1)*10, 
          "gems worth", sum(sample(1:6, 2, replace = TRUE)), "coins each")
  )
  
  if (loot_roll < 15) {
    return(qsample(loot))
  }else{
    return(qsample(loot_and_reroll))
  }
  
}
# Creature generation -----------------------------------------------------


roll_were_creature <- function() {
  were_creature_type <- "Beast"
  
  if (were_creature_type == "Beast") {
    were_creature_category <- qsample(
      c(
        rep("Earthbound", 7),
        rep("Airborne", 3),
        rep("Aquatic", 2)
      )
    )
    
    if (were_creature_category == "Earthbound") {
      were_creature_specific <- qsample(
        c(
          "termite/tick/louse",
          "snail/slug/worm",
          "ant/centipede/scorpion",
          "snake/lizard",
          "vole/rat/weasel",
          "boar/pig",
          "dog/fox/wolf",
          "cat/lion/panther",
          "deer/horse/camel",
          "ox/rhino",
          "bear/ape/gorilla",
          "mammoth/dinosaur"
        )
      )
    }
    
    if (were_creature_category == "Airborne") {
      were_creature_specific <- qsample(
        c(
          "mosquito/firefly",
          "locust/dragonfly/moth",
          "bee/wasp",
          "chicken/duck/goose",
          "songbird/parrot",
          "gull/waterbird",
          "heron/crane/stork",
          "crow/raven",
          "hawk/falcon",
          "eagle/owl",
          "condor",
          "pteranodon"
        )
      )
    }
    
    if (were_creature_category == "Aquatic") {
      were_creature_specific <- qsample(
        c(
          "insect ",
          "jelly/anemone",
          "clam/oyster/snail",
          "eel/snake",
          "frog/toad",
          "fish",
          "crab/lobster",
          "turtle",
          "alligator/crocodile",
          "dolphin/shark",
          "squid/octopus",
          "whale"
        )
      )
    }
  }
  return(were_creature_specific)
}

roll_beast <- function(creature_category = "random") {
  
  if (creature_category == "random") {
    
    creature_category <- qsample(
      c(
        rep("Earthbound", 7),
        rep("Airborne", 3),
        rep("Aquatic", 2)
      )
    )
  }
  
  if (!creature_category %in% c("Earthbound", "Airborne", "Aquatic") ) {
    stop(paste("invalid creature category:", creature_category))
  }
  
  if (creature_category == "Earthbound") {
    creature_specific <- qsample(
      c(
        "termite/tick/louse",
        "snail/slug/worm",
        "ant/centipede/scorpion",
        "snake/lizard",
        "vole/rat/weasel",
        "boar/pig",
        "dog/fox/wolf",
        "cat/lion/panther",
        "deer/horse/camel",
        "ox/rhino",
        "bear/ape/gorilla",
        "mammoth/dinosaur"
      )
    )
  }
  
  if (creature_category == "Airborne") {
    creature_specific <- qsample(
      c(
        "mosquito/firefly",
        "locust/dragonfly/moth",
        "bee/wasp",
        "chicken/duck/goose",
        "songbird/parrot",
        "gull/waterbird",
        "heron/crane/stork",
        "crow/raven",
        "hawk/falcon",
        "eagle/owl",
        "condor",
        "pteranodon"
      )
    )
  }
  
  if (creature_category == "Aquatic") {
    creature_specific <- qsample(
      c(
        "insect ",
        "jelly/anemone",
        "clam/oyster/snail",
        "eel/snake",
        "frog/toad",
        "fish",
        "crab/lobster",
        "turtle",
        "alligator/crocodile",
        "dolphin/shark",
        "squid/octopus",
        "whale"
      )
    )
  }
  
  creature_additional_details <- list(
    "Activity"      = roll_details("Activity"),
    "Disposition"   = roll_details("Disposition"),
    "No. Appearing" = roll_details("No. Appearing"),
    "Size"          = roll_details("Size")
  )
  
  
  return(
    list(
      "creature type"     = "Beast",
      "category" = creature_category,
      "specific" = creature_specific,
      "details"  = creature_additional_details
    )
  )
}

roll_humanoid <- function(creature_category = "random") {
  
  if (creature_category == "random") {
    creature_category <- qsample(
      c(
        rep("Common", 7),
        rep("Uncommon", 3),
        rep("Hybrid", 2)
      )
    )
  }
  
  if (creature_category == "Common") {
    creature_specific <- qsample(
      c(
        rep("Halfling (Small)", 3),
        rep("goblin/kobold (Small)", 2),
        rep("dwarf/gnome (Small)", 2),
        rep("orc/hobgoblin/gnoll", 2),
        rep("half-elf, half-orc, etc", 2),
        "elf"
      )
    )
  }
  
  if (creature_category == "Uncommon") {
    creature_specific <- qsample(
      c(
        "fey (Tiny)",
        rep("catfolk/dogfolk", 2),
        rep("lizardfolk/merfolk", 2),
        "birdfolk",
        rep("ogre/troll (Large)", 3),
        rep("cyclops/giant", 2)
      )
    )
  }
  
  
  
  
  if (creature_category == "Hybrid") {
    creature_specific <- qsample(
      c(
        rep("centaur", 2),
        rep("werewolf/werebear", 3),
        paste0("werecreature (human/", roll_were_creature(), ")"),
        rep(paste("human with", roll_were_creature()),3),
        rep(paste("human with two", paste0(roll_were_creature(), "s")),2)
      )
    )
  }
  
  creature_additional_details <- c(
    roll_npc(),
    list(
      "Disposition"   = roll_details("Disposition"),
      "No. Appearing" = roll_details("No. Appearing"))
  )
  
  return(
    list(
      "creature type"     = "Humanoid",
      "category" = creature_category,
      "specific" = creature_specific,
      "details"  = creature_additional_details
    )
  )
}

roll_human <- function() {
  creature_category <- "Human"
  creature_specific <- "Human"
  creature_additional_details <- c(
    roll_npc(),
    list("Disposition"   = roll_details("Disposition"),
         "No. Appearing" = roll_details("No. Appearing"))
  )
  
  return(
    list(
      "creature type"     = "Human",
      "category" = creature_category,
      "specific" = creature_specific,
      "details"  = creature_additional_details
    )
  )
}

roll_monster <- function(creature_category = "random") {
  
  if (creature_category == "random") {
    creature_category <- qsample(
      c(
        rep("Unusual", 7),
        rep("Rare", 3),
        rep("Legendary", 2)
      )
    )
  }
  
  if (!creature_category %in% c("Unusual", "Rare", "Legendary")) {
    stop(paste("invalid creature category:", creature_category))
  }
  
  if (creature_category == "Unusual") {
    creature_specific <- qsample(
      c(
        rep("plant/fungus", 3),
        rep("Undead Human", 2),
        rep("Undead Humanoid", 1),
        rep(paste(roll_were_creature(), "+", roll_were_creature()), 2),
        rep(paste(roll_were_creature(), "that can", roll_details("Ability")), 2),
        rep(paste0(roll_were_creature(), " (", roll_details("Feature"), ")"), 2)
      )
    )
  }
  
  if (creature_category == "Rare") {
    creature_specific <- qsample(
      c(
        rep("slime/ooze (Amorphous)", 3),
        rep("creation (Construct)", 3),
        rep(paste(roll_details("Oddity"), roll_were_creature()), 3)
        #TODO:unnatural entity
        
      )
    )
  }
  
  if (creature_category == "Legendary") {
    creature_specific <- qsample(
      c(
        rep("dragon/colossus (Huge)", 3),
        rep(paste(qsample(
          c(
            rep("plant/fungus", 3),
            rep("Undead Human", 2),
            rep("Undead Humanoid", 1),
            rep(paste(roll_were_creature(), "+", roll_were_creature()), 2),
            rep(paste(roll_were_creature(), "that can", roll_details("Ability")), 2),
            rep(paste0(roll_were_creature(), " (", roll_details("Feature"), ")"), 2)
          )
        ), "(Huge)"), 2),
        rep(paste(qsample(
          c(
            rep("slime/ooze (Amorphous)", 3),
            rep("creation (Construct)", 3),
            rep(paste(roll_details("Oddity"), roll_were_creature()), 3)
            #TODO:unnatural entity
            
          )
        ), "(Huge)"), 2),
        rep(paste0(roll_were_creature(), "-dragon" ), 1),
        paste0(qsample(
          c(
            rep("plant/fungus", 3),
            rep("Undead Human", 2),
            rep("Undead Humanoid", 1),
            rep(paste(roll_were_creature(), "+", roll_were_creature()), 2),
            rep(paste(roll_were_creature(), "that can", roll_details("Ability")), 2),
            rep(paste0(roll_were_creature(), " (", roll_details("Feature"), ")"), 2)
          )
        ), " + Dragon"),
        paste0(qsample(
          c(
            rep("slime/ooze (Amorphous)", 3),
            rep("creation (Construct)", 3),
            rep(paste(roll_details("Oddity"), roll_were_creature()), 3)
            #TODO:unnatural entity
            
          )
        ), " with a Dragon")
      )
    )
  }
  
  creature_additional_details <- list(
    "Activity" = roll_details("Activity"),
    "Alignment" = roll_details("Alignment"),
    "No. Appearing" = roll_details("No. Appearing"), 
    "Disposition" = roll_details("Disposition"),
    "Size" = roll_details("Size"),
    "Optional attributes:" = list(
      "Ability" = roll_details("Ability"),
      "Adjective" = roll_details("Adjective"),
      "Age" = roll_details("Age"),
      "Aspect" = roll_details("Aspect"),
      "Condition" = roll_details("Condition"),
      "Feature" = roll_details("Feature"),
      "Tag" = roll_details("Tag")
    )
  )
  
  return(
    list(
      "creature type"     = "Monster",
      "category" = creature_category,
      "specific" = creature_specific,
      "details"  = creature_additional_details
    )
  )
}

roll_creature <- function(creature_type = "random") {
  
  if (creature_type == "random") {
    creature_type <- qsample(
      c(
        rep("Beast", 4),
        rep("Human", 2),
        rep("Humanoid", 2),
        rep("Monster", 4)
      )
    )
  }
  
  if (!creature_type %in% c("Beast", "Humanoid", "Human", "Monster")) {
    stop(paste("invalid creature type:", creature_type))
  }
  
  if (creature_type == "Beast") {
    creature_temp <- roll_beast(creature_category = "random")
  }
  
  
  if (creature_type == "Humanoid") {
    creature_temp <- roll_humanoid(creature_category = "random")
  }
  
  if (creature_type == "Human") {
    creature_temp <- roll_human()
  }
  
  if (creature_type == "Monster") {
    creature_temp <- roll_monster()
    
  }
  
  return(
    creature_temp
  )
  
}


roll_npc_occupation <- function(occupation_type = NA) {
  if (is.na(occupation_type)) {
    occupation_type <- qsample(
      c(
        "Criminal",
        rep("Commoner", 5),
        rep("Tradesman", 2),
        rep("Merchant", 2),
        "Specialist",
        "Official"
      )
    )
  }
  
  if (occupation_type == "Criminal") {
    occupation <- qsample(
      c(
        "bandit/brigand/thug",
        "bandit/brigand/thug",
        "thief",
        "thief",
        "bodyguard/tough",
        "bodyguard/tough",
        "burglar",
        "burglar",
        "dealer/fence",
        "racketeer",
        "lieutenant",
        "boss"
      )
    )
  }
  
  if (occupation_type == "Commoner") {
    occupation <- qsample(
      c(
        "housewife/husband",
        "hunter/gatherer",
        "hunter/gatherer",
        "farmer/herder",
        "farmer/herder",
        "farmer/herder",
        "laborer/servant",
        "laborer/servant",
        "driver/porter/guide",
        "sailor/soldier/guard",
        "clergy/monk",
        "apprentice/adventurer"
      )
    )
  }
  
  if (occupation_type == "Tradesperson") {
    occupation <- qsample(
      c(
        "cobbler/furrier/tailor",
        "weaver/basketmaker",
        "potter/carpenter",
        "mason/baker/chandler",
        "cooper/wheelright",
        "tanner/ropemaker",
        "smith/tinker",
        "stablekeeper/herbalist",
        "vintner/jeweler",
        "innkeeper/tavernkeeper",
        "artist/actor/minstrel",
        "armorer/weaponsmith"
      )
    )
  }
  
  if (occupation_type == "Merchant") {
    occupation <- qsample(
      c(
        "general goods/outfitter",
        "general goods/outfitter",
        "general goods/outfitter",
        "raw materials",
        "grain/livestock",
        "ale/wine/spirits",
        "clothing/jewelry",
        "weapons/armor",
        "spices/tobacco",
        "labor/slaves",
        "books/scrolls",
        "magic supplies/items"
      )
    )
  }
  
  if (occupation_type == "Specialist") {
    occupation <- qsample(
      c(
        "undertaker",
        "sage/scholar/wizard",
        "writer/illuminator",
        "perfumer",
        "architect/engineer",
        "locksmith/clockmaker",
        "physician/apothecary",
        "navigator/guide",
        "alchemist/astrologer",
        "spy/diplomat",
        "cartographer",
        "inventor"
      )
    )
  }
  
  if(occupation_type == "Official") {
    occupation <- qsample(
      c(
        "town crier",
        "tax collector",
        "armiger/gentry",
        "armiger/gentry",
        "reeve/sherrif/constable",
        "mayor/magistrate",
        "priest/bishop/abbot",
        "guildmaster",
        "knight/templar",
        "elder/high priest",
        "noble (baron etc.)",
        "lord/lady/king/queen"
      )
    )
  }
  
  return(paste0(tolower(occupation_type), ": ", occupation))
}

roll_npc_trait <- function(trait_category = "random") {
  
  if (trait_category == "random") {
    trait_category <- qsample(
      c(
        rep("Physical Appearance", 6),
        rep("Personality", 3),
        rep("Quirk", 2)
      )
    )
  }
  
  if (!trait_category %in% c("Physical Appearance", "Personality", "Quirk")) {
    stop(paste("Invalid trait category:", trait_category))
  }
  
  if (trait_category == "Physical Appearance" ) {
    trait <- qsample(
      c(
        "disfigured (missing teeth, eye, etc.)",
        "lasting injury (bad leg, arm, etc.)",
        "tattooed/pockmarked/scarred",
        "unkempt/shabby/grubby",
        "big/thick/brawny",
        "small/scrawny/emaciated",
        "notable hair (wild, long, none, etc.)",
        "notable nose (big, hooked, etc.)",
        "notable eyes (blue, bloodshot, etc.)",
        "clean/well-dressed/well-groomed",
        "clean/well-dressed/well-groomed",
        "attractive/handsome/stunning"
      )
    )
  }
  
  if (trait_category == "Personality") {
    trait <- qsample(
      c(
        "loner/alienated/antisocial ",
        "cruel/belligerent/bully",
        "anxious/fearful/cowardly",
        "envious/covetous/greedy",
        "aloof/haughty/arrogant",
        "awkward/shy/self-loathing",
        "orderly/compulsive/controlling",
        "confident/impulsive/reckless ",
        "kind/generous/compassionate",
        "easygoing/relaxed/peaceful",
        "easygoing/relaxed/peaceful",
        "cheerful/happy/optimistic"
      )
    )
  }
  
  if (trait_category == "Quirk") {
    trait <- qsample(
      c(
        "insecure/racist/xenophobic",
        "addict (sweets, drugs, sex, etc.)",
        "phobia (spiders, fire, darkness, etc.)",
        "allergic/asthmatic/chronically ill ",
        "skeptic/paranoid",
        "superstitious/devout/fanatical",
        "miser/pack-rat",
        "spendthrift/wastrel",
        "smart aleck/know-it-all",
        "artistic/dreamer/delusional",
        "naive/idealistic"
      )
    )
  }
  
  return(paste0(trait_category, ": ", trait))
}

roll_npc <- function(context = "random") {
  
  if (context == "random") {
    qsample(
      c("Wilderness", "Rural", "Urban")
    )
  }
  if (context == "Wilderness") {
    npc_type <- qsample(
      c(
        roll_npc_occupation("Criminal"),
        roll_npc_occupation("Criminal"),
        "adventurer/explorer",
        "adventurer/explorer",
        "hunter/gatherer",
        "hunter/gatherer",
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        "ranger/scout",
        "ranger/scout",
        "soldier/mercenary",
        roll_npc_occupation("Official")
      )
    )
    
    npc_trait     <- roll_npc_trait()
    npc_activity  <- roll_details("Activity")
    npc_alignment <- roll_details("Alignment")
    
  }
  
  if (context == "Rural") {
    npc_type <- qsample(
      c(
        "beggar/urching",
        roll_npc_occupation("Criminal"),
        "adventurer/explorer",
        "hunter/gatherer",
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Tradesperson"),
        roll_npc_occupation("Merchant"),
        "militia/soldier/guard",
        roll_npc_occupation("Official")
      )
    )
  }
  
  if (context == "Urban") {
    npc_type <- qsample(
      c(
        "beggar/urchin",
        "beggar/urching",
        roll_npc_occupation("Criminal"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Commoner"),
        roll_npc_occupation("Tradesperson"),
        roll_npc_occupation("Merchant"),
        roll_npc_occupation("Specialist"),
        "militia/soldier/guard",
        roll_npc_occupation("Official"),
      )
    )
    
  }
  
  
  return(
    list(
      "NPC"       = npc_type, 
      "Alignment" = npc_alignment, 
      "Trait"     = npc_trait, 
      "Activity"  = npc_activity   
    )
    
  )
}


# Discovery generation ----------------------------------------------------
roll_feature_template <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c()
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes
  
  
  # end subtypes  
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
  
}


roll_unnatural_feature <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c(
    rep("Arcane", 9),
    rep("Planar", 2),
    rep("Divine", 1)
  )
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes start
  if (subtype_found == "Arcane") {
    core_feature <- qsample(
      c(
        rep("residue",             2),
        rep("blight",              3),
        rep("alteration/mutation", 2),
        rep("enchantment",         2),
        rep("source/repository",   2)
      ) 
    )
    
    bonus_features <- list("Alignment"  = roll_details("Alignment"), 
                           "Magic type" = roll_details("Magic Type"))
  }
  
  if (subtype_found == "Planar") {
    core_feature <- qsample(
      c(
        rep("distortion/warp", 4),
        rep("portal/gate",     4),
        rep("rift/tear",       2),
        rep("outpost",         2)
      ) 
    )
    
    bonus_features <- list("Alignment" = roll_details("Alignment"), 
                           "Element"   = roll_details("Element"))
  }
  
  if (subtype_found == "Divine") {
    core_feature <- qsample(
      c(
        rep("mark/sign",      3),
        rep("cursed place",   3),
        rep("hallowed place", 3),
        rep("watched place",  2),
        rep("presence"),
      ) 
    )
    
    bonus_features <- list("Alignment" = roll_details("Alignment"), 
                           "Aspect"    = roll_details("Aspect"))
  }
  # subtypes end
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
}

roll_natural_feature <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c(
    rep("Lair",           2),
    rep("Obstacle",       2),
    rep("Terrain Change", 3),
    rep("Water Feature",  2),
    rep("Landmark",       2),
    rep("Resource",       1)
  )
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes start
  
  if (subtype_found == "Lair") {
    core_feature <- qsample(
      c(
        rep("burrow",       3),
        rep("cave/tunnels", 4),
        rep("nest/aerie",   2),
        rep("hive",         1),
        rep("ruins",        2)
      )
    )
    
    if (core_feature == "ruins") {
      # Ruins generate their own creature, so we don't need to add it here.
      bonus_features <- list(
        "Visibility" = roll_details("Visibility"),
        "Structure"  = roll_structure("Ruin")
      )
    }else{
      bonus_features <- list(
        "Visibility" = roll_details("Visibility"),
        "Creature"   = roll_creature()
      )
    }
  }
  
  if (subtype_found == "Obstacle") {
    core_feature <- qsample(
      c(
        rep("difficult ground",     5),
        rep("cliff/crevasse/chasm", 3),
        rep("ravine/gorge",         2),
        rep(roll_details("Oddity"), 2)
      )
    )
    
    bonus_features <- list(
      "Optional A" = roll_details("Oddity"),
      "Optional B" = roll_details("Ruination")
    )
  }
  
  
  
  if (subtype_found == "Terrain Change") {
    core_feature <- qsample(
      c(
        rep(paste("limited area:", roll_details("Terrain")), 4),
        rep("crevice/hole/pit/cave", 2),
        rep("altitude change",       2),
        rep("canyon/valley",         2),
        rep("rise/peak in distance", 2)
      )
    )
    
    bonus_features <- list(
      "Optional A" = roll_details("Oddity"),
      "Optional B" = roll_details("Terrain")
    )
  }
  
  if (subtype_found == "Water Feature") {
    core_feature <- qsample(
      c(
        rep("spring/hotspring",   1),
        rep("waterfall/geyser",   1),
        rep("creek/stream/brook", 4),
        rep("pond/lake",          2),
        rep("river",              2),
        rep("sea/ocean",          2)
      )
    )
    
    bonus_features <- list(
      "Optional A" = roll_beast("Aquatic"),
      "Optional B" = roll_details("Oddity")
    )
  }
  
  if (subtype_found == "Landmark") {
    core_feature <- qsample(
      c(
        rep("water-based (waterfall, geyser, etc.)", 3),
        rep("plant-based (ancient tree, giant flowers, etc.)", 3),
        rep("earth-based (peak, rock formation, crater, etc.", 4),
        rep(roll_details("Oddity"))
      )
    )
    
    bonus_features <- list(
      "Optional A" = roll_details("Adjective"),
      "Optional B" = roll_details("Oddity")
    )
  }
  
  if (subtype_found == "Resource") {
    core_feature <- qsample(
      c(
        rep("game/fruit/vegetable",     4),
        rep("herb/spice/dye source",    2),
        rep("timber/stone",             3),
        rep("ore (copper, iron, etc.)", 2),
        rep("precious metal/gems",      1)
      )
    )
    
    bonus_features <- list(
      "Size"       = roll_details("Size"),
      "Visibility" = roll_details("Visibility")
    )
  }
  # subtypes end 
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
  
}

roll_evidence_feature <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c(
    rep("Tracks/Spoor",   6),
    rep("Remains/Debris", 4),
    rep("Stash/Cache",    2)
  )
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes
  if (subtype_found == "Tracks/Spoor") {
    core_feature <- qsample(
      c(
        rep("faint/unclear",        3),
        rep("definite/clear",       3),
        rep("multiple",             2),
        rep("signs of violence",    2),
        rep("trail of blood/other", 2)
      )
    )
    
    bonus_features <- list(
      "Age"                  = roll_details("Age"),
      "Creature responsible" = roll_creature()
    )
  }
  
  if (subtype_found == "Remains/Debris") {
    core_feature <- qsample(
      c(
        rep("bones",               4),
        rep("corpse/carcass",      3),
        rep("site of violence",    1),
        rep("junk/refuse",         1),
        rep("lost supplies/cargo", 1),
        rep("tools/weapons/armor", 1)
      )
    )
    
    bonus_features <- list(
      "Age"        = roll_details("Age"),
      "Visibility" = roll_details("Visibility")
    )
  }
  
  if (subtype_found == "Stash/Cache") {
    core_feature <- qsample(
      c(
        rep("trinkets/coins",      3),
        rep("tools/weapons/armor", 2),
        rep("map",                 2),
        rep("food/supplies",       2),
        rep(roll_treasure(),       2)
      )
    )
  }
  
  # end subtypes  
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
  
}

roll_structure <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c(
    rep("Enigmatic",        1),
    rep("Infrastructure",   2),
    rep("Dwelling",         1),
    rep("Burial/Religious", 2),
    rep("Steading",         2),
    rep("Ruin",             4)
  )
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes
  if (subtype_found == "Enigmatic") {
    core_feature <- qsample(
      c(
        rep("earthworks",           4),
        rep("megalith",             4),
        rep("statue/idol/totem",    3),
        rep(roll_details("Oddity"), 1)
      )
    )
    
    bonus_features <- list(
      "Age"  = qsample(c("Old", "Ancient", "prehistoric")),
      "Size" = qsample(c(rep("medium-sized", 6),
                         rep("large", 2),
                         "huge")),
      "Visibility" = roll_details("Visibility")
    )
  }
  
  if (subtype_found == "Infrastructure") {
    core_feature <- qsample(
      c(
        rep("track/path",            4),
        rep("road",                  4),
        rep("bridge/ford",           2),
        rep("mine/quarry",           1),
        rep("aqueduct/canal/portal", 1)
      )
    )
    
    bonus_features <- list(
      "Creature responsible" = roll_creature(qsample(c("Human", "Humanoid")))
    )
  }
  
  if (subtype_found == "Dwelling") {
    core_feature <- qsample(
      c(
        rep("campsite",          3),
        rep("hovel/hut",         3),
        rep("farm",              2),
        rep("inn/roadhouse",     2),
        rep("tower/keep/estate", 2)
      )
    )
    
    bonus_features <- list(
      "Creature responsible" = roll_creature(qsample(c("Human", "Humanoid")))
    )
  }
  
  if (subtype_found == "Burial/Religious") {
    core_feature <- qsample(
      c(
        rep("grave marker/barrow",  2),
        rep("graveyard/necropolis", 2),
        rep("tomb/crypt",           2),
        rep("shrine",               3),
        rep("temple/retreat",       2),
        rep("great temple",         1)
      )
    )
    
    bonus_features <- list(
      "Creature responsible" = roll_creature(qsample(c("Human", "Humanoid"))),
      "Alignment"            = roll_details("Alignment"),
      "Aspect"               = roll_details("Aspect")
    )
  }
  
  if (subtype_found == "Steading") {
    core_feature <- qsample(
      c(
        roll_steading()
      )
    )
    
    bonus_features <- list(
      "Creature responsible" = roll_creature(qsample(c("Human", "Humanoid")))
    )
  }
  
  if (subtype_found == "Ruin") {
    core_feature <- qsample(
      c(
        rep(qsample(
          c(
            rep("track/path",            4),
            rep("road",                  4),
            rep("bridge/ford",           2),
            rep("mine/quarry",           1),
            rep("aqueduct/canal/portal", 1)
          )
        ), 2),
        rep(qsample(
          c(
            rep("campsite",          3),
            rep("hovel/hut",         3),
            rep("farm",              2),
            rep("inn/roadhouse",     2),
            rep("tower/keep/estate", 2)
          )
        ), 2),
        rep(qsample(
          c(
            rep("grave marker/barrow",  2),
            rep("graveyard/necropolis", 2),
            rep("tomb/crypt",           2),
            rep("shrine",               3),
            rep("temple/retreat",       2),
            rep("great temple",         1)
          )
        ), 2),
        rep(roll_steading(), 2),
        rep(roll_dungeon(),  2)
      )
    )
    
    bonus_features <- list(
      "Creature responsible" = roll_creature(qsample(c("Human", "Humanoid"))),
      "Age"                  = qsample(c("Old", "Ancient", "prehistoric")),
      "Ruination"            = roll_details("Ruination"),
      "Visibility"           = roll_details("Visibility")
    )
  }
  
  # end subtypes  
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
  
}


roll_discovery <- function() {
  discovery_types <- c(
    rep("Unnatural Feature", 1),
    rep("Natural Feature",   3),
    rep("Evidence",          2),
    rep("Creature",          2)
    #rep("Structure",         4)
  )
  
  discovered <- qsample(discovery_types)
  
  if (discovered == "Unnatural Feature") {
    discovery <- roll_unnatural_feature()
  }
  
  if (discovered == "Natural Feature") {
    discovery <- roll_natural_feature()
  }
  
  if (discovered == "Evidence") {
    discovery <- roll_evidence_feature()
  }
  
  if (discovered == "Creature") {
    discovery <- roll_creature()
  }
  
  if (discovered == "Structure") {
    discovery <- roll_structure()
  }
  
  return(c(list("Type" = discovered), discovery))
}


# Danger generation -------------------------------------------------------

roll_danger <- function(danger_type = "random") {
  
  danger_types <- c(
    rep("Unnatural entity", 1),
    rep("Hazard",           5),
    rep("Creature",         6)
  )
  
  if(!danger_type %in% c(danger_types, "random")) {
    stop(paste("invalid feature subtype:", danger_type))
  }
  
  # test for random subtype generation
  if(danger_type == "random") {
    danger_type_found <- qsample(danger_types)
  }else{
    danger_type_found <- danger_type 
  }
  
  
  # subtypes
  if (danger_type_found == "Unnatural entity") {
    discovery <- roll_unnatural_entity()
  }
  
  if (danger_type_found == "Hazard") {
    discovery <- roll_hazard()
  }
  
  if (danger_type_found == "Creature") {
    discovery <- roll_creature()
  }
  # end subtypes
  
  return(c(list("Type" = danger_type_found), discovery))
}

roll_unnatural_entity <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c(
    rep("Undead", 8),
    rep("Planar", 3),
    rep("Divine", 1)
  )
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes
  if (subtype_found == "Undead") {
    core_feature <- qsample(
      c(
        rep("haunt/wisp",         4),
        rep("ghost/spectre",      4),
        rep("banshee",            1),
        rep("wraith/wight",       2),
        rep("spirit lord/master", 1)
      )
    )
    
    bonus_features <- list(
      "Ability"     = roll_details("Ability"),
      "Activity"    = roll_details("Activity"),
      "Alignment"   = roll_details("Alignment"),
      "Disposition" = roll_details("Disposition")
    )
  }
  
  if (subtype_found == "Planar") {
    core_feature <- qsample(
      c(
        rep("imp (small)",          3),
        rep("lesser elemental",     3),
        rep("lesser demon/horror",  3),
        rep("greater elemental",    1),
        rep("greater demon/horror", 1),
        rep("devil/elemental lord", 1)
      )
    )
    
    bonus_features <- list(
      "Ability"     = roll_details("Ability"),
      "Activity"    = roll_details("Activity"),
      "Alignment"   = roll_details("Alignment"),
      "Disposition" = roll_details("Disposition"),
      "Element"     = roll_details("Element"),
      "Feature"     = roll_details("Feature"),
      "Tag"         = roll_details("Tag")
    )
  }
  
  if (subtype_found == "Divine") {
    core_feature <- qsample(
      c(
        rep("agent",        5),
        rep("champion",     4),
        rep("army (horde)", 2),
        rep("avatar",       1)
      )
    )
    
    bonus_features <- list(
      "Ability"     = roll_details("Ability"),
      "Activity"    = roll_details("Activity"),
      "Alignment"   = roll_details("Alignment"),
      "Aspect"      = roll_details("Aspect"),
      "Disposition" = roll_details("Disposition"),
      "Element"     = roll_details("Element"),
      "Feature"     = roll_details("Feature"),
      "Tag"         = roll_details("Tag")
    )
  }
  # end subtypes
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
}


roll_hazard <- function(subtype = "random") {
  # list of accepted subtypes
  subtypes <- c(
    rep("Unnatural", 2),
    rep("Natural",   8),
    rep("Trap",      2)
  )
  
  # input validation
  if(!subtype %in% c(subtypes, "random")) {
    stop(paste("invalid feature subtype:", subtype))
  }
  
  # test for random subtype generation
  if(subtype == "random") {
    subtype_found <- qsample(subtypes)
  }else{
    subtype_found <- subtype 
  }
  
  # subtypes
  if (subtype_found == "Unnatural") {
    core_feature <- qsample(
      c(
        rep("taint/blight/curse", 3),
        rep("arcane tra[/effect", 5),
        rep("planar trap/effect", 3),
        rep("divine", 1)
      )
    )
    
    bonus_features <- list(
      "Aspect"     = roll_details("Aspect"),
      "Visibility" = roll_details("Visibility")
    )
  }
  
  if (subtype_found == "Natural") {
    core_feature <- qsample(
      c(
        rep("blinding mist/fog",      2),
        rep("bog/mire/quicksand",     2),
        rep("pitfall/sinkhole/chasm", 3),
        rep("poison/disease",         2),
        rep("flood/fire/tornado",     2),
        rep(roll_details("Oddity"),  1)
      )
    )
    
    bonus_features <- list(
      "Size" = roll_details("Size")
    )
  }
  
  if (subtype_found == "Trap") {
    core_feature <- qsample(
      c(
        rep("alarm",                     2),
        rep("ensnaring/paralyzing",      3),
        rep("injurious (pitfall, etc.)", 3),
        rep("gas/fire/poison",           1),
        rep("ambush",                    1)
      )
    )
    
    bonus_features <- list(
      "Creature responsible" = roll_creature(),
      "Aspect"               = roll_details("Aspect"),
      "Visibility"           = roll_details("Visibility")
    )
  }
  
  # end subtypes  
  
  # output
  return(
    list(
      "subtype"        = subtype_found,
      "core feature"   = core_feature,
      "extra features" = bonus_features
    )
  )
  
}

# Steading generation -----------------------------------------------------

roll_steading <- function() {
  
}



# Dungeon generation ------------------------------------------------------

roll_dungeon_theme <- function(type = "random") {
  
  # Acceptable type inputs
  types <- c(
    rep("Mundane",       5),
    rep("Unusual",       4),
    rep("Extraordinary", 3)
  )
  
  # input validation
  if (!type %in% c(types, "random")) {
    stop(paste("invalid theme type input:", type))
  }
  
  if (type == "random") {
    type_found <- qsample(types)
  }else{
    type_found <- type
  }
  
  if (type_found == "Mundane") {
    theme_found <- qsample(
      c(
        "rot/decay",
        "torture/agony",
        "madness",
        "all is lost",
        "noble sacrifice",
        "savage fury",
        "survival",
        "criminal activity",
        "secrets/treachery",
        "tricks and traps",
        "invasion/infestation",
        "factions at war"
      )
    )
  }
  
  if (type_found == "Unusual") {
    theme_found <- qsample(
      c(
        "creation/invention",
        roll_details("Element"),
        "knowledge/expansion",
        "deepening mystery",
        "transformation/change",
        "chaos and destruction",
        "shadowy forces",
        "forbidden knowledge",
        "poison/disease",
        "corruption/blight",
        "impending disaster"
      )
    )
  }
  
  if (type_found == "Extraordinary") {
    theme_found <- qsample(
      c(
        "scheming evil",
        "divination/scrying",
        "blasphemy",
        "arcane research",
        "occult forces",
        "an ancient curse",
        "mutation",
        "the unquiet dead",
        "bottomless hunger",
        "incredible power",
        "unspeakable horrors",
        "holy war"
      )
    )
  }
  
  return(
    theme_found
  )
}

roll_dungeon_discovery <- function(type = "random") {
  
  # Acceptable type inputs
  types <- c(
    rep("Dressing",3),
    rep("Feature", 4),
    rep("Find",    3)
  )
  
  # input validation
  if (!type %in% c(types, "random")) {
    stop(paste("invalid discovery type input:", type))
  }
  
  if (type == "random") {
    type_found <- qsample(types)
  }else{
    type_found <- type
  }
  
  if (type_found == "Dressing") {
    discovery <- qsample(
      c(
        "junk/debris",
        "tracks/marks",
        "signs of battle",
        "writing/carving",
        "warning",
        paste("dead", roll_creature()$specific),
        "bones/remains",
        "book/scroll/map",
        "broken door/wall",
        "breeze/wind/smell",
        "lichen/moss/fungus",
        roll_details("Oddity")
      )
    )
  }
  
  if (type_found == "Feature") {
    discovery <- qsample(
      c(
        "cave-in/collapse",
        "pit/shaft/chasm",
        "locked door/gate",
        "alcoves/niches",
        "brdige/stairs/ramp",
        "fountain/well/pool",
        "puzzle",
        "altar/dais/platform",
        "statue/idol",
        "magic pool/statue/idol",
        "connection to another dungeon"
      )
    )
  }
  
  if (type_found == "Find") {
    discovery <- qsample(
      c(
        "trinkets",
        "tools",
        "weapons/armor",
        "supplies/trade goods",
        "coins/gems/jewelry",
        "poisons/potions",
        "adventurer/captive",
        "magic item",
        "scroll/book",
        "magic weapon/armor",
        "artifact",
        paste(sample(c("trinkets",
                       "tools",
                       "weapons/armor",
                       "supplies/trade goods",
                       "coins/gems/jewelry",
                       "poisons/potions",
                       "adventurer/captive",
                       "magic item",
                       "scroll/book",
                       "magic weapon/armor",
                       "artifact"), 2), collapse = ", "
              
        )
      )
    )
  }
  
  return(
    list(
      "class" = "Discovery",
      "type"  = type_found,
      "details" = discovery
    )
    
  )
}

roll_dungeon_danger <- function(type = "random") {
  
  # Acceptable type inputs
  types <- c(
    rep("Trap",     4),
    rep("Creature", 7),
    rep("Entity",   1)
  )
  
  # input validation
  if (!type %in% c(types, "random")) {
    stop(paste("invalid discovery type input:", type))
  }
  
  if (type == "random") {
    type_found <- qsample(types)
  }else{
    type_found <- type
  }
  
  if (type_found == "Trap") {
    danger_found <- qsample(
      c(
        "alarm",
        "ensnaring/paralyzing",
        "pit",
        "crushing",
        "piercing/puncturing",
        "chopping/slashing",
        "confusing (maze, etc.)",
        "gas (poison, etc.)",
        roll_details("Element"),
        "ambush",
        paste(qsample(c("alarm",
                        "ensnaring/paralyzing",
                        "pit",
                        "crushing",
                        "piercing/puncturing",
                        "chopping/slashing",
                        "confusing (maze, etc.)",
                        "gas (poison, etc.)",
                        roll_details("Element"),
                        "ambush")), 
              qsample(c("alarm",
                        "ensnaring/paralyzing",
                        "pit",
                        "crushing",
                        "piercing/puncturing",
                        "chopping/slashing",
                        "confusing (maze, etc.)",
                        "gas (poison, etc.)",
                        roll_details("Element"),
                        "ambush")), sep = ", ")
      )
    )
  }
  
  if (type_found == "Creature") {
    danger_found <- qsample(
      c(
        "waiting in ambush   ",
        "fighting/squabbling",
        "prowling/on patrol",
        "looking for food  ",
        "eating/resting",
        "guarding",
        "on the move",
        "searching/scavenging",
        "returning to den",
        "making plans",
        "sleeping",
        "dying"
      )
    )
    
    dungeon_creature <- roll_creature()
    dungeon_creature$details$Activity <- danger_found
  }
  
  if (type_found == "Entity") {
    danger_found <- qsample(
      c(
        "alien interloper",
        "vermin lord",
        "criminal mastermind",
        "warlord ",
        "high priest",
        "oracle",
        "wizard/witch/alchemist",
        paste("Monster lord:", roll_creature("Monster")$specific),
        "evil spirit/ghost ",
        "undead lord (lich, etc.)",
        "demon",
        "dark god"
      )
    )
  }
  
  if (type_found != "Creature") {
    return(
      list(
        "class" = "Danger",
        "type"  = type_found,
        "details" = danger_found
      )
    )
  }else{
    return(
      list(
        "class" = "Danger",
        "type"  = type_found,
        "details" = dungeon_creature
      )
    )
  }
}


roll_dungeon <- function(size = "random") {
  
  # Acceptable size inputs
  sizes <- c(
    rep("small",  3),
    rep("medium", 6),
    rep("large",  2),
    rep("huge",   1)
  )
  
  # input validation
  if (!size %in% c(sizes, "random")) {
    stop(paste("Invalid dungeon size:", size))
  }
  
  # test if random dungeon size was requested (default)
  if (size == "random") {
    dungeon_size <- qsample(sizes)
  }else{
    dungeon_size <- size
  }
  
  # roll areas and themes
  if (dungeon_size == "small") {
    area_count  <- qsample(1:6) + 2
    theme_count <- qsample(1:4)
  }
  
  if (dungeon_size == "medium") {
    area_count  <- qsample(1:6) + qsample(1:6) + 2
    theme_count <- qsample(1:6)
  }
  
  if (dungeon_size == "large") {
    area_count  <- qsample(1:6) + qsample(1:6) + qsample(1:6) + 2
    theme_count <- qsample(1:6) + 1
  }
  
  if (dungeon_size == "huge") {
    area_count  <- qsample(1:6) + qsample(1:6) + qsample(1:6) + qsample(1:6) + 2
    theme_count <- qsample(1:6) + 2
  }
  
  # Clip theme count if area number too small to support it
  if (area_count < theme_count) {
    theme_count <- area_count
  }
  
  dungeon_builder <- qsample(
    c(
      rep("aliens/precursors",      1),
      rep("demigod/demon",          1),
      rep("natural (caves, etc.)",  2),
      rep("religious order/cult",   1),
      rep(roll_humanoid()$specific, 2),
      rep("dwarves/gnomes",         2),
      rep("elves",                  1),
      rep("wizard/madman",          1),
      rep("monarch/warlord",        1)
    )
  )
  
  dungeon_function <- qsample(
    c(
      rep("source/portal",        1),
      rep("mine",                 1),
      rep("tomb/crypt",           2),
      rep("prison",               1),
      rep("lair/den/hideout",     2),
      rep("stronghold/sanctuary", 2),
      rep("shrine/temple/oracle", 1),
      rep("archive/library",      1),
      rep("unknown/mystery",      1)
    )
  )
  
  dungeon_ruination <- roll_details("Ruination")
  
  dungeon_themes <- list()
  
  for( i in 1:theme_count) {
    
    dungeon_themes[[i]] <- list(
      "theme" = roll_dungeon_theme(),
      "countdown" = theme_count
    )
    
    
  }
  
  
  return(
    list(
      "Size"      = dungeon_size,
      "Builder"   = dungeon_builder,
      "Function"  = dungeon_function,
      "Ruination" = dungeon_ruination,
      "Areas"     = area_count,
      "Themes"    = dungeon_themes,
      "Entrance"  = roll_details("Visibility")
    )
  )
  
}


# Automatic dungeon exploration -------------------------------------------

explore_dungeon <- function(dungeon = "new") {
  
  if (class(dungeon) != "list") {
    dungeon <- roll_dungeon()
  }
  
  # Theme table
  theme_table <- data.frame(stringsAsFactors = FALSE)
  for (p in 1:length(dungeon$Themes)) {
    added_row <- data.frame(
      "theme" = dungeon$Themes[[p]]$theme,
      "count" = dungeon$Themes[[p]]$countdown,
      stringsAsFactors = FALSE
    )
    theme_table <-rbind(theme_table, added_row)
  }
  
  # set up some counters and containers
  limit <- dungeon$Areas * 2
  rooms <- list()
  room_count  <- 1
  theme_limit <- FALSE
  
  while (length(rooms) < limit & theme_limit == FALSE) {
    grabbed_room <- roll_room()
    
    if (grabbed_room$themed == TRUE) {
      if (sum(theme_table$count > 0)) {
        grabbed_room$theme <- qsample(theme_table[theme_table$count > 0, 1])
        theme_table$count[theme_table$theme == grabbed_room$theme] <- theme_table$count[theme_table$theme == grabbed_room$theme] - 1
        if (sum(theme_table$count) == 0) {
          theme_limit <- TRUE
        }
      }
    } else {
      grabbed_room$theme <- "unthemed"
    }
    
    grabbed_room$themed <- NULL
    rooms[[room_count]] <- grabbed_room
    room_count <- room_count + 1
    
  }
  
  return(rooms)
  
}

roll_room <- function() {
  found <- qsample(1:12)
  
  finds <- list(
    list("themed" = FALSE, "area type" = "Common", "contents" = "empty"),
    list("themed" = FALSE, "area type" = "Common", "contents" = list(roll_dungeon_danger())),
    list("themed" = FALSE, "area type" = "Common", "contents" = list(roll_dungeon_discovery(), 
                                                                     roll_dungeon_danger())),
    list("themed" = FALSE, "area type" = "Common", "contents" = list(roll_dungeon_discovery(), 
                                                                     roll_dungeon_danger())),
    list("themed" = FALSE, "area type" = "Common", "contents" = list(roll_dungeon_discovery())),
    list("themed" = FALSE, "area type" = "Common", "contents" = list(roll_dungeon_discovery())),
    list("themed" = TRUE,  "area type" = "Common", "contents" = list(roll_dungeon_danger())),
    list("themed" = TRUE,  "area type" = "Common", "contents" = list(roll_dungeon_danger(), 
                                                                     roll_dungeon_discovery())),
    list("themed" = TRUE,  "area type" = "Common", "contents" = list(roll_dungeon_discovery())),
    list("themed" = TRUE,  "area type" = "Unique", "contents" = list(roll_dungeon_danger())),
    list("themed" = TRUE,  "area type" = "Unique", "contents" = list(roll_dungeon_danger(), 
                                                                     roll_dungeon_discovery())),
    list("themed" = TRUE,  "area type" = "Unique", "contents" = list(roll_dungeon_discovery()))
  )
  
  
  return(finds[[found]])
}


# Discord formatting ------------------------------------------------------

discord_dungeon <- function(dungeon) {
  
}

discord_creature <- function(creature) {
  separator_thick = "================================="
  separator_thin  = "------------------------------------------------------"
  
  basics <- paste(
    separator_thick,
    "**MONSTER**",
    separator_thick,
    paste0("*",creature$`creature type`, " • " ,creature$category, " • " ,creature$specific, "*"),
    separator_thin,
    sep = "\n"
  )
  details_names <- names(creature$details)
  
  if (any(details_names == "Optional attributes:")) {
    details_names <- details_names[-which((details_names == "Optional attributes:"))]
    opt_det <- TRUE
  }else{
    opt_det <- FALSE
  }
  
  details_names <- paste0("***", details_names, ":***")
  
  detail_print <- vector(mode = "character")
  for (i in 1:length(details_names)) {
    detail_print <- c(detail_print, paste(details_names[i], creature$details[[i]]))
  }
  
  detail_print <- paste(c(detail_print, separator_thin) , collapse = "\n")
  
  printout <- paste0(basics,"\n", detail_print)
  
  if (opt_det == TRUE) {
    opt_det_names <- names(creature$details$`Optional attributes:`)
    opt_det_values <- creature$details$`Optional attributes:`
    printout <- paste0(printout,"\n**Optional Attributes:**\n", 
                       paste(paste0("***", opt_det_names, ": ***", opt_det_values), collapse = "\n"), "\n",separator_thick)
  }
  
  return(printout)
}
