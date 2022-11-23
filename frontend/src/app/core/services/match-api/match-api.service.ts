import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
@Injectable({
  providedIn: 'root',
})
export class MatchApiService {
  private MATCHED_API = `${environment.apiBaseURL}matched/`;

  constructor(private http: HttpClient) {}

  public getMatched(user_id: string): Observable<any> {
    return this.http.get<any>(this.MATCHED_API + `${user_id}`);
  }
}
