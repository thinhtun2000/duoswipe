import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserPreferencesCardComponent } from './user-preferences-card.component';

describe('UserPreferencesCardComponent', () => {
  let component: UserPreferencesCardComponent;
  let fixture: ComponentFixture<UserPreferencesCardComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UserPreferencesCardComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(UserPreferencesCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
