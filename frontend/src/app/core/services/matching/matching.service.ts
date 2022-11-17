import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';
import { MatchingObject } from '../../models/matchingObject';
@Injectable({
  providedIn: 'root',
})
export class MatchingService {
  private MATCHING_API = `${environment.apiBaseURL}matched_update`;

  constructor(private http: HttpClient) {}

  public update_match(matchingObject: MatchingObject): Observable<any> {
    return this.http.post<any>(this.MATCHING_API, matchingObject);
  }
}
