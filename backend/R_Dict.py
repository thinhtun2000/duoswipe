# Avoid querying the database

rank_ref = {"Iron": 1,
            "Bronze": 2,
            "Silver": 3,
            "Gold": 4,
            "Platinum": 5,
            "Diamond": 6,
            "Master": 7,
            "Grandmaster": 8,
            "Challenger": 9}


position_ref = {"Tank": 1,
                "Bruiser": 2,
                "Jungler": 3,
                "APC": 4,
                "ADC": 5,
                "Support": 6}


location_ref = {"Las Cruces": 1,
                "El Paso": 2}


language_ref = {"English": 1,
                "Spanish": 2}


def get_key(dictionary, value):
    return [k for k, v in dictionary.items() if v == value]