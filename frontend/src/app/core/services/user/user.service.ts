import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';
import { User } from '../../models/user';

@Injectable({
  providedIn: 'root',
})
export class UserService {
  public _user: BehaviorSubject<User | null> = new BehaviorSubject<User | null>(
    null
  );

  get userSnapshot(): User | null {
    return this._user.value;
  }

  get user$(): Observable<User | null> {
    return this._user.asObservable();
  }

  constructor() {}

  public setUser(user: User | null) {
    this._user.next(user);
  }
}
