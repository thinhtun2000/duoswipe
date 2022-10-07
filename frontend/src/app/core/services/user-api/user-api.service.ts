import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { User } from '../../models/user';

@Injectable({
  providedIn: 'root',
})
export class UserApiService {
  constructor(private http: HttpClient) {}

  public getUserById(id: string): Observable<User> {
    return this.http.get<User>(`http://127.0.0.1:5000/profile/${id}`);
  }
}
