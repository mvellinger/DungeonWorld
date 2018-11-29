
# Generator functions -----------------------------------------------------


# Helpers -----------------------------------------------------------------

# qsample() quickly samples 1 item from any vector (saves typing)
qsample <- function(x) {
  sample(x, 1)
}



# Region name -------------------------------------------------------------

name_region <- function() {
  
  require(feather)
  
  names_list <- read_feather("./data/region_names.feather")
  names(names_list) <- c("terrain", "adjective", "noun")
  
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
        rep("Evil", 3),
        rep("Neutral", 4),
        rep("Good", 2),
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
        rep("Solitary (1)", 4),
        rep(paste0("Group (", sample(1:6, 1)+2, ")" ), 5),
        rep(paste0("Horde (", sum(sample(1:6, 4)), " per wave)" ), 3)
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

# Discovery ---------------------------------------------------------------

make_creature <- function() {
  creature_type <- qsample(
    c(
      rep("Beast", 4),
      rep("Human", 2),
      rep("Humanoid", 2),
      rep("Monster", 4)
    )
  )
  
  if (creature_type == "Beast") {
    creature_hint <- "Start with a real-world creature, then put a spin on it"
    creature_category <- qsample(
      c(
        rep("Earthbound", 7),
        rep("Airborne", 3),
        rep("Water-going", 2)
      )
    )
    
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
    
    if (creature_category == "Water-going") {
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
    
    creature_additional_details <- paste(
      roll_details("Activity"),
      roll_details("Disposition"),
      roll_details("No. Appearing"),
      roll_details("Size"), sep = ", "
    )
  }
  
  if (creature_type == "Humanoid") {
    creature_hint <- "If you roll a classic fantasy species, adapt it to your setting"
    creature_category <- qsample(
      c(
        rep("Common", 7),
        rep("Uncommon", 3),
        rep("Hybrid", 2)
      )
    )
    
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
    
    ww_creature <- function() {
      ww_creature_type <- "Beast"
      
      if (ww_creature_type == "Beast") {
        ww_creature_hint <- "Start with a real-world creature, then put a spin on it"
        ww_creature_category <- qsample(
          c(
            rep("Earthbound", 7),
            rep("Airborne", 3),
            rep("Water-going", 2)
          )
        )
        
        if (ww_creature_category == "Earthbound") {
          ww_creature_specific <- qsample(
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
        
        if (ww_creature_category == "Airborne") {
          ww_creature_specific <- qsample(
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
        
        if (ww_creature_category == "Water-going") {
          ww_creature_specific <- qsample(
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
      return(ww_creature_specific)
    }
    
    
    if (creature_category == "Hybrid") {
      creature_specific <- qsample(
        c(
          rep("centaur", 2),
          rep("werewolf/werebear", 3),
          paste0("werecreature (human/", ww_creature()),
          rep(paste("human with", ww_creature()),3),
          rep(paste("human with two", paste0(ww_creature(), "s")),2)
        )
      )
    }
    
    creature_additional_details <- paste(
      roll_details("Activity"),
      roll_details("Alignment"),
      roll_details("Disposition"),
      roll_details("No. Appearing")
      #TODO roll_npc_details()
      , sep = ", ")
  }
  
  if (creature_type == "Human") {
    creature_hint <- "Humans, the most dangerous beasts."
    creature_category <- roll_details("Age")
    creature_specific <- roll_details("Ability")
    creature_additional_details <- paste(
      roll_details("Activity"),
      roll_details("Alignment"),
      roll_details("Disposition"),
      roll_details("No. Appearing")
      #TODO roll_npc_details()
      , sep = ", ")
    
  }
  
  if (creature_type == "Monster") {
    creature_hint <- "Give every monster life!"
    creature_category <- qsample(
      c(
        rep("Unusual", 7),
        rep("Rare", 3),
        rep("Legendary", 2)
      )
    )
    
    if (creature_category == "Unusual") {
      creature_specific <- qsample(
        c(
          rep("plant/fungus", 3),
          rep("Undead Human", 2),
          rep("Undead Humanoid", 1),
          rep(paste(ww_creature(), "+", ww_creature()), 2),
          rep(paste(ww_creature(), "that can", roll_details("Ability")), 2),
          rep(paste0(ww_creature(), " (", roll_details("Feature"), ")"), 2)
        )
      )
    }
    
    if (creature_category == "Rare") {
      creature_specific <- qsample(
        c(
          rep("slime/ooze (Amorphous)", 3),
          rep("creation (Construct)", 3),
          rep(paste(roll_details("Oddity"), ww_creature()), 3)
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
             rep(paste(ww_creature(), "+", ww_creature()), 2),
             rep(paste(ww_creature(), "that can", roll_details("Ability")), 2),
             rep(paste0(ww_creature(), " (", roll_details("Feature"), ")"), 2)
           )
         ), "(Huge)"), 2),
         rep(paste(qsample(
           c(
             rep("slime/ooze (Amorphous)", 3),
             rep("creation (Construct)", 3),
             rep(paste(roll_details("Oddity"), ww_creature()), 3)
             #TODO:unnatural entity
             
           )
         ), "(Huge)"), 2),
         rep(paste0(ww_creature(), "-dragon" ), 1),
         paste0(qsample(
           c(
             rep("plant/fungus", 3),
             rep("Undead Human", 2),
             rep("Undead Humanoid", 1),
             rep(paste(ww_creature(), "+", ww_creature()), 2),
             rep(paste(ww_creature(), "that can", roll_details("Ability")), 2),
             rep(paste0(ww_creature(), " (", roll_details("Feature"), ")"), 2)
           )
         ), " with a Dragon"),
         paste0(qsample(
           c(
             rep("slime/ooze (Amorphous)", 3),
             rep("creation (Construct)", 3),
             rep(paste(roll_details("Oddity"), ww_creature()), 3)
             #TODO:unnatural entity
             
           )
         ), " with a Dragon")
        )
      )
    }
    
    creature_additional_details <- paste(
      roll_details("Activity"),
      roll_details("Alignment"),
      roll_details("Disposition"),
      roll_details("No. Appearing"),
      roll_details("Size"),
      "\nOptionally:\n",
      roll_details("Ability"),
      roll_details("Adjective"), 
      roll_details("Age"),
      roll_details("Aspect"),
      roll_details("Condition"),
      roll_details("Feature"),
      roll_details("Tags"),
      sep = ", "
    )
  }
  
  return(
    list(
      creature_type,
      creature_hint,
      creature_category,
      creature_specific,
      creature_additional_details
    )
  )
  
}

make_discovery <- function() {
  discovery_types <- c(
    "Unnatural Feature",
    "Natural Feature",
    "Natural Feature",
    "Natural Feature",
    "Evidence",
    "Evidence",
    "Creature",
    "Creature",
    "Structure",
    "Structure",
    "Structure",
    "Structure"
  )
  
  discovered <- qsample(discovery_types)
  
  if (discovered == "Unnatural Feature") {
    subtype <- c(
      rep("Arcane", 9),
      rep("Planar", 2),
      "Divine"
    )
    subtype_found <- qsample(subtype)
    
    if (subtype_found == "Arcane") {
      core_feature <- qsample(
        c(
          rep("residue", 2),
          rep("blight", 3),
          rep("alteration/mutation", 2),
          rep("enchantment", 2),
          rep("source/repository"),
        ) 
      )
      bonus_features <- paste(roll_details("Alignment"), 
                              roll_details("Magic Type"), 
                              sep = ", ")
    }
    
    if (subtype_found == "Planar") {
      core_feature <- qsample(
        c(
          rep("distortion/warp", 4),
          rep("portal/gate", 4),
          rep("rift/tear", 2),
          rep("outpost", 2)
        ) 
      )
      bonus_features <- paste(roll_details("Alignment"), 
                              roll_details("Element"), 
                              sep = ", ")
    }
    
    if (subtype_found == "Divine") {
      core_feature <- qsample(
        c(
          rep("mark/sign", 3),
          rep("cursed place", 3),
          rep("hallowed place", 3),
          rep("watched place", 2),
          rep("presence"),
        ) 
      )
      bonus_features <- paste(roll_details("Alignment"), 
                              roll_details("Aspect"), 
                              sep = ", ")
    }
  }
  
  if (discovered == "Natural Feature") {
    
    subtype <- c(
      rep("Lair", 2),
      rep("Obstacle", 2),
      rep("Terrain Change", 3),
      rep("Water Feature", 2),
      rep("Landmark", 2),
      rep("Resource")
    )
    subtype_found <- qsample(subtype)
    
    
  }
  
  if (discovered == "Evidence") {}
  
  if (discovered == "Creature") {}
  
  if (discovered == "Structure") {}
  
  
}
