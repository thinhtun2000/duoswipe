from connToDB import db


# Table 'champ'
class Champ(db.Model):
    __tablename__ = 'champ'
    champ_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(32))
    sprite = db.Column(db.VARCHAR(256))

    def __repr__(self):
        return 'Champ %r' % self.champ_id


# Insert into 'champ'
def create_champ(champ_id=None, name=None, sprite=None):
    champ = Champ()
    champ.champ_id = champ_id
    champ.name = name
    champ.sprite = sprite

    db.session.add(champ)
    db.session.commit()
