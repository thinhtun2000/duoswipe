from flask import Flask, render_template, request
from app import db


# Table 'rank'
class Rank(db.Model):
    __tablename__ = 'rank'
    rank_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))
    sprite = db.Column(db.VARCHAR(256))

    def __repr__(self):
        return 'Rank %r' % self.rank_id


# Insert into 'rank'
def create_rank(rank_id=None, name=None, sprite=None):
    rank = Rank()
    rank.rank_id = rank_id
    rank.name = name
    rank.sprite = sprite

    db.session.add(rank)
    db.session.commit()
