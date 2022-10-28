from connToDB import db


# Table 'location'
class Location(db.Model):
    __tablename__ = 'location'
    location_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))

    def __repr__(self):
        return 'Location %r' % self.location_id


# Insert into 'location'
def create_loc(location_id=None, name=None):
    location = Location()
    location.location_id = location_id
    location.name = name

    db.session.add(location)
    db.session.commit()
