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

import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Element;
import com.google.gwt.event.dom.client.ScrollEvent;
import com.google.gwt.event.dom.client.ScrollHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.widget.client.base.AbstractComposite;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class NavSpy extends AbstractComposite implements HasDrawable {

	private static final CssStyle STYLE_NAV_SPY = new SimpleStyle("nav-spy");

	private class NavWidget extends NavLink implements ScheduledCommand {
		private final NavWidget parentNav;
		private final int level;
		private Nav subNavContainer;

		public NavWidget(NavWidget parentNav, int level) {
			this.parentNav = parentNav;
			this.level = level;
		}

		public NavWidget(NavWidget parentNav, final Heading heading) {
			super(heading.getText(), new ScheduledCommand() {
				@Override
				public void execute() {
					int top = getElementTop(heading) - spyOffset;
					getScrollElement().setScrollTop(top);
				}
			});

			this.parentNav = parentNav;
			this.level = heading.getLevel();
		}

		private NavWidget addToSubNav(NavWidget subNav) {
			getSubNavContainer().append(subNav);
			return subNav;
		}

		public Nav getSubNavContainer() {
			if (subNavContainer == null) {
				subNavContainer = new Nav();
				append(subNavContainer);
			}
			return subNavContainer;
		}

		@Override
		public void execute() {
		}
	}

	private final SimplePanel content = new SimplePanel();

	private final List<Heading> headings = Lists.newArrayList();
	private final Map<Heading, NavWidget> navs = Maps.newHashMap();

	private boolean refreshing = false;
	private HandlerRegistration scrollRegistration;
	private Widget scrollWidget;

	private int spyOffset;

	private final RepeatingCommand refreshCommand = new RepeatingCommand() {

		@Override
		public boolean execute() {
			refreshActive();
			return false;
		}
	};

	public NavSpy() {
		initWidget(content);
		StyleUtils.addStyle(this, STYLE_NAV_SPY);
	}

	protected NavSpy(NavSpy source) {
		super(source);
		initWidget(content);
	}

	@Override
	public IsWidget cloneWidget() {
		return new NavSpy(this);
	}

	private void refreshActive() {
		if (navs.isEmpty()) {
			return; // Not displayed NavSpy
		}
		Element scrollElement = getScrollElement();
		int scrollTop = scrollElement.getScrollTop() + spyOffset;
		int scrollHeight = scrollElement.getScrollHeight();
		int maxScroll = scrollHeight - scrollElement.getClientHeight();

		Heading activeHeading = null;
		if (scrollTop >= maxScroll && headings.size() > 0) {
			activeHeading = headings.get(headings.size() - 1);
		}
		else {
			for (Heading heading : headings) {
				int top = getElementTop(heading) - scrollTop;
				if (activeHeading == null || top <= 0) {
					activeHeading = heading;
				}
				if (top > 0) {
					break;
				}
			}
		}

		for (NavWidget nav : navs.values()) {
			nav.setActive(false);
		}
		if (activeHeading != null) {
			NavWidget navActive = navs.get(activeHeading);
			navActive.setActive(true);
			while (navActive.parentNav != null) {
				navActive = navActive.parentNav;
				navActive.setActive(true);
			}
		}
		refreshing = false;
	}

	public void addHeading(Heading heading) {
		if (heading != null) {
			headings.add(heading);
		}
	}

	public void setScrollWidget(IsWidget scrollWidget) {
		if (scrollWidget != null) {
			this.scrollWidget = scrollWidget.asWidget();
		}
		else {
			this.scrollWidget = null;
		}
		registerScrollHandler();
	}

	public void setSpyOffset(int spyOffset) {
		this.spyOffset = spyOffset;
	}

	@Override
	protected void onLoad() {
		super.onLoad();
		registerScrollHandler();
	}

	@Override
	protected void onUnload() {
		super.onUnload();
		if (scrollRegistration != null) {
			scrollRegistration.removeHandler();
		}
	}

	private void registerScrollHandler() {
		if (scrollRegistration != null) {
			scrollRegistration.removeHandler();
		}
		if (this.scrollWidget == null) {
			scrollRegistration = Window.addWindowScrollHandler(new Window.ScrollHandler() {

				@Override
				public void onWindowScroll(Window.ScrollEvent event) {
					scheduleRefresh();
				}
			});
		}
		else {
			scrollRegistration = scrollWidget.addDomHandler(new ScrollHandler() {

				@Override
				public void onScroll(ScrollEvent event) {
					scheduleRefresh();
				}
			}, ScrollEvent.getType());
		}
	}

	private void scheduleRefresh() {
		if (!refreshing) {
			refreshing = true;
			Scheduler.get().scheduleFixedDelay(refreshCommand, 250);
		}
	}

	private Element getScrollElement() {
		if (scrollWidget != null) {
			return scrollWidget.getElement();
		}
		return RootPanel.get().getElement();
	}

	private int getElementTop(Heading heading) {
		if (scrollWidget == null) {
			return heading.getElement().getAbsoluteTop();
		}
		else {
			return heading.getElement().getOffsetTop() - scrollWidget.getElement().getOffsetTop();
		}
	}

	public List<Heading> getHeadings() {
		return Collections.unmodifiableList(headings);
	}

	@Override
	public void redraw() {
		int lowestNavLevel = 6;
		navs.clear();
		content.clear();
		for (Heading heading : headings) {
			if (heading.getLevel() < lowestNavLevel) {
				lowestNavLevel = heading.getLevel();
			}
		}

		NavWidget currentNav = new NavWidget(null, lowestNavLevel - 1);
		content.add(currentNav.getSubNavContainer());
		for (Heading heading : headings) {
			while (currentNav.level >= heading.getLevel() && currentNav.parentNav != null) {
				currentNav = currentNav.parentNav;
			}
			if (currentNav.level < heading.getLevel() - 1) {
				for (int i = currentNav.level; i < heading.getLevel(); i++) {
					NavWidget newNav = new NavWidget(currentNav, i);
					currentNav.append(newNav);
					currentNav = newNav;
				}
			}

			currentNav = currentNav.addToSubNav(new NavWidget(currentNav, heading));
			navs.put(heading, currentNav);
		}
	}
}
