import { Component, ElementRef, OnInit } from '@angular/core';
import { Point } from '../../models/point';
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
  public users: User[];
  public index: number = 0;
  public offsetX: number = 0;
  public offsetY: number = 0;
  public static startPoint: any;
  public userToDisplay1: User;
  public userToDisplay2: User;

  constructor(
    private swipeSvc: SwipeService,
    private userSvc: UserService,
    private userApiSvc: UserApiService,
    private element: ElementRef,
    private matching: MatchingService
  ) {}

  ngOnInit(): void {
    this.userSvc.user$.subscribe((user) => {
      this.user = user;
      console.log(this.user);
    });
    this.swipeSvc.getUsers(this.user?.user_id).subscribe((response) => {
      this.users = response;
      console.log(response);
    });
    const like = document.getElementById('like')!;
    const nope = document.getElementById('nope')!;
    like.style.opacity = '0';
    nope.style.opacity = '0';
  }

  public fetchNextUsers(id1: string, id2: string) {
    this.userApiSvc.getUserById(id1).subscribe((user) => {
      this.userToDisplay1 = user;
    });
    this.userApiSvc.getUserById(id2).subscribe((user) => {
      this.userToDisplay2 = user;
    });
  }

  public handleMousePress(event: MouseEvent) {
    SwipePageComponent.startPoint = { x: event.x, y: event.y };
    // Mouse could be up before it ever moves
    document.addEventListener('mouseup', this.handleMouseUp);
    document.addEventListener('mousemove', this.handleMouseMove, true);
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
      else nope.style.opacity = `${opacity}`;
    }
  }

  public handleMouseUp(event: MouseEvent) {
    const card = document.getElementById('first')!;
    const like = document.getElementById('like')!;
    const nope = document.getElementById('nope')!;
    console.log(event.x);
    if (
      event.x > SwipePageComponent.startPoint.x &&
      event.x - SwipePageComponent.startPoint.x > 400
    ) {
      console.log('right');
      () => this.handleSwipeRight();
    } else if (
      event.x < SwipePageComponent.startPoint.x &&
      SwipePageComponent.startPoint.x - event.x > 400
    ) {
      console.log('left');
      () => this.handleSwipeLeft();
    } else {
      SwipePageComponent.startPoint = null;
      document.removeEventListener('mousemove', this.handleMouseMove, true);
      document.getElementById('first')!.style.transform = '';
      like.style.opacity = `0`;
      nope.style.opacity = `0`;
    }
  }

  public handleSwipeRight(): void {
    console.log('swiped right');
    // register the match
    // remove the card
    // fetch new users[]
  }

  public handleSwipeLeft(): void {
    console.log('swipe left');
    // remove the card
    // fetch new users[]
  }
}
