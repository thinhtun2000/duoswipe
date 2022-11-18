import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';
import { User } from '../../models/user';

@Injectable({
  providedIn: 'root',
})
export class UserService {
  // user
  public _user: BehaviorSubject<User | null> = new BehaviorSubject<User | null>(
    null
  );
  get userSnapshot(): User | null {
    return this._user.value;
  }
  get user$(): Observable<User | null> {
    return this._user.asObservable();
  }

  // list of users to match with
  public _users: BehaviorSubject<Array<string>> = new BehaviorSubject(['']);
  get usersSnapshot(): Array<string> {
    return this._users.value;
  }
  get users$(): Observable<Array<string>> {
    return this._users.asObservable();
  }

  constructor() {}

  public setUser(user: User | null) {
    this._user.next(user);
  }

  public setUsers(users: Array<string>) {
    this._users.next(users);
  }
}
