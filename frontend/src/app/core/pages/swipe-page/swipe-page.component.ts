import { Component, ElementRef, OnInit } from '@angular/core';
import { BehaviorSubject, map } from 'rxjs';
import { User } from '../../models/user';
import { MatchingService } from '../../services/matching/matching.service';
import { SwipeService } from '../../services/swipe/swipe.service';
import { UserApiService } from '../../services/user-api/user-api.service';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-swipe-page',
  templateUrl: './swipe-page.component.html',
  styleUrls: ['./swipe-page.component.scss'],
})
export class SwipePageComponent implements OnInit {
  public user: User | null;
  public users: Array<string>;
  public offsetX: number = 0;
  public offsetY: number = 0;
  public static startPoint: any;
  public toMatch1: User;
  public toMatch2: User;

  constructor(
    private swipeSvc: SwipeService,
    private userSvc: UserService,
    private userApi: UserApiService,
    private matching: MatchingService
  ) {}

  ngOnInit(): void {
    // fetch info of the user who is swiping
    this.userSvc.user$.subscribe((user) => {
      this.user = user;
      console.log(this.user);
      this.userSvc.users$.subscribe((users) => {
        this.users = users;
        console.log(this.users);
        this.userApi.getUserById(this.users[0]).subscribe((user) => {
          this.toMatch1 = user;
        });
        this.userApi.getUserById(this.users[1]).subscribe((user) => {
          this.toMatch2 = user;
        });
      });
    });
  }

  public handleMouseDown(event: MouseEvent) {
    SwipePageComponent.startPoint = { x: event.x, y: event.y };
  }

  public handleMouseMove(event: MouseEvent) {
    const card = document.getElementById('first')!;
    const like = document.getElementById('like')!;
    const nope = document.getElementById('nope')!;
    if (SwipePageComponent.startPoint != null) {
      this.offsetX = event.x - SwipePageComponent.startPoint.x;
      this.offsetY = event.y - SwipePageComponent.startPoint.y;
      const rotate = this.offsetX * 0.1;
      card.style.transform = `translate(${this.offsetX}px, ${this.offsetY}px) rotate(${rotate}deg)`;
      // if swipe right then show Like tag, if left show Pass tag
      const opacity = Math.abs(this.offsetX / (card.clientWidth * 0.4));
      if (this.offsetX > 0) like.style.opacity = `${opacity}`;
      else if (this.offsetX == 0) {
        like.style.opacity = '0';
        nope.style.opacity = '0';
      } else nope.style.opacity = `${opacity}`;
    }
  }

  public handleMouseUp(event: MouseEvent) {
    const card = document.getElementById('first')!;
    const like = document.getElementById('like')!;
    const nope = document.getElementById('nope')!;
    if (
      event.x > SwipePageComponent.startPoint.x &&
      event.x - SwipePageComponent.startPoint.x > 250
    ) {
      this.swipeRight();
    } else if (
      event.x < SwipePageComponent.startPoint.x &&
      SwipePageComponent.startPoint.x - event.x > 250
    ) {
      this.swipeLeft();
    }
    SwipePageComponent.startPoint = null;
    document.removeEventListener('mousemove', this.handleMouseMove, true);
    document.getElementById('first')!.style.transform = '';
    like.style.opacity = `0`;
    nope.style.opacity = `0`;
  }

  public swipeRight() {
    console.log('right');
    const matchObject = {
      current_user: this.user?.user_id,
      to_match_user: this.users[0],
    };
    console.log(matchObject);

    this.matching.update_match(matchObject).subscribe((response) => {
      console.log(response);
    });
  }

  public swipeLeft() {
    console.log('swipe left');
    this.users.shift();
    console.log(this.users);
  }
}
