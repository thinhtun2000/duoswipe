import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
import { User } from '../../models/user';

@Injectable({
  providedIn: 'root',
})
export class SwipeService {
  private SWIPE_API = `${environment.apiBaseURL}match`;

  constructor(private http: HttpClient) {}

  public getUsers(user_id: any): Observable<any> {
    return this.http.get<any>(`${this.SWIPE_API}/${user_id}`);
  }
}
