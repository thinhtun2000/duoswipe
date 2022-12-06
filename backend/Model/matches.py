from connToDB import db


# Table 'matches'
class Match(db.Model):
    __tablename__ = 'matches'
    match_id = db.Column(db.INTEGER, primary_key=True, autoincrement=True)
    user_id_1 = db.Column(db.INTEGER)
    user_id_2 = db.Column(db.INTEGER)
    user1_match = db.Column(db.BOOLEAN)
    user2_match = db.Column(db.BOOLEAN)
    match_h = db.Column(db.BOOLEAN)

    def __repr__(self):
        return 'Match %r' % self.match_id


# Insert into 'match'
def create_match(user_id_1=None, user_id_2=None, user1_match=None, user2_match=None, match_h=None ):
    match = Match()
    match.user_id_1 = user_id_1
    match.user_id_2 = user_id_2
    match.user1_match = user1_match
    match.user2_match = user2_match
    match.match_h = match_h

    db.session.add(match)
    db.session.commit()


