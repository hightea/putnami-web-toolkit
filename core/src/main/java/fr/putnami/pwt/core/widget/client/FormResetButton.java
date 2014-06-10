/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.Driver;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent.HasDirtyHandlers;
import fr.putnami.pwt.core.editor.client.event.ResetDisplayEvent;
import fr.putnami.pwt.core.editor.client.event.ResetDisplayEvent.HasResetDisplayHandlers;
import fr.putnami.pwt.core.model.client.base.HasDriver;
import fr.putnami.pwt.core.widget.client.event.ButtonEvent;

public class FormResetButton<T> extends Button<T> implements HasDriver<T, Driver<T>> {

	private final ButtonEvent.Handler buttonHandler = new ButtonEvent.Handler() {
		@Override
		public void onButtonAction(ButtonEvent event) {
			driver.resetDisplay();
		}
	};
	private final DirtyEvent.Handler dirtyHandler = new DirtyEvent.Handler() {
		@Override
		public void onDirtyEvent(DirtyEvent event) {
			setDisabled(false);
		}
	};
	private final ResetDisplayEvent.Handler resetDisplayHandler = new ResetDisplayEvent.Handler() {
		@Override
		public void onResetDisplayEvent(ResetDisplayEvent event) {
			setDisabled(true);
		}
	};

	private Driver<T> driver;

	public FormResetButton() {
		setDisabled(true);
	}

	public FormResetButton(FormResetButton<T> source) {
		super(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new FormResetButton<T>(this);
	}

	@Override
	public Driver<T> getDriver() {
		return driver;
	}

	@Override
	public void setDriver(Driver<T> driver) {
		this.driver = driver;
		initEvents();
	}

	private void initEvents() {
		addButtonHandler(buttonHandler);
		Editor rootEditor = driver.getRootEditor();
		if (rootEditor instanceof HasDirtyHandlers) {
			((HasDirtyHandlers) rootEditor).addDirtyHandler(dirtyHandler);
		}
		if (rootEditor instanceof HasResetDisplayHandlers) {
			((HasResetDisplayHandlers) rootEditor).addResetDisplayHandler(resetDisplayHandler);
		}
	}
}
