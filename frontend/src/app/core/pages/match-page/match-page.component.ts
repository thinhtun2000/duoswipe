import { Component, OnInit } from '@angular/core';
import { User } from '../../models/user';
import { MatchApiService } from '../../services/match-api/match-api.service';
import { UserApiService } from '../../services/user-api/user-api.service';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-match-page',
  templateUrl: './match-page.component.html',
  styleUrls: ['./match-page.component.scss'],
})
export class MatchPageComponent implements OnInit {
  public user: User | null;
  public matched: any;

  constructor(
    private userSvc: UserService,
    private userApi: UserApiService,
    private matchApi: MatchApiService
  ) {}

  ngOnInit(): void {
    this.userSvc.user$.subscribe((user) => {
      this.user = user;
      console.log(this.user!.user_id);
      this.matchApi.getMatched(this.user!.user_id).subscribe((response) => {
        this.matched = response;
        console.log(this.matched);
      });
    });
  }
}
