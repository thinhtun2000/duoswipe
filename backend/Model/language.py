from flask import Flask, render_template, request
from app import db


# Table 'Language'
class Language(db.Model):
    __tablename__ = 'position'
    language_id = db.Column(db.INTEGER, primary_key=True)
    name = db.Column(db.VARCHAR(16))

    def __repr__(self):
        return 'Language %r' % self.language_id


# Insert into 'language'
def create_language(language_id=None, name=None):
    language = Language()
    language.language_id = language_id
    language.name = name

    db.session.add(language)
    db.session.commit()
