import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
import { LoginRequest } from '../../models/loginRequest';
import { RegisterRequest } from 'src/app/core/models/registerRequest';

@Injectable({
  providedIn: 'root',
})
export class AuthApiService {
  private AUTH_API = `${environment.apiBaseURL}`;

  constructor(private http: HttpClient) {}

  public login(loginRequestBody: LoginRequest): Observable<any> {
    return this.http.post<any>(`${this.AUTH_API}login`, loginRequestBody);
  }
  public register(registerRequestBody: RegisterRequest): Observable<any> {
    return this.http.post<any>(`${this.AUTH_API}login`, registerRequestBody);
  }
}
