import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
import { User } from '../../models/user';

@Injectable({
  providedIn: 'root',
})
export class UserApiService {
  private USER_API = `${environment.apiBaseURL}profile`;

  constructor(private http: HttpClient) {}

  public getUserById(id: string): Observable<User> {
    return this.http.get<User>(`${this.USER_API}/${id}`);
  }

  public updateUser(id: string, user: User): Observable<any> {
    return this.http.post<any>(`${this.USER_API}/${id}`, user);
  }
}
