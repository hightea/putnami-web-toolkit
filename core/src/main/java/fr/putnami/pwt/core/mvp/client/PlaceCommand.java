package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.place.shared.Place;

public class PlaceCommand implements ScheduledCommand {

	private final Place place;

	public PlaceCommand(Place place) {
		super();
		this.place = place;
	}

	@Override
	public void execute() {
		MvpController.get().goTo(this.place);
	}

}
