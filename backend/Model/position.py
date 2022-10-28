from app import db


# Table 'position'
class Position(db.Model):
    __tablename__ = 'position'
    position_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))
    sprite = db.Column(db.VARCHAR(32))

    def __repr__(self):
        return 'Position %r' % self.position_id


# Insert into 'position'
def create_position(position_id=None, name=None, sprite=None):
    position = Position()
    position.position_id = position_id
    position.name = name
    position.sprite = sprite

    db.session.add(position)
    db.session.commit()
