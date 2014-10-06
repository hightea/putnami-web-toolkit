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

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.gwt.core.client.Scheduler;
import com.google.gwt.core.client.Scheduler.RepeatingCommand;
import com.google.gwt.core.client.Scheduler.ScheduledCommand;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.Node;
import com.google.gwt.event.dom.client.ScrollEvent;
import com.google.gwt.event.dom.client.ScrollHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.gwt.user.client.ui.Widget;

import java.util.List;
import java.util.Map;

import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractComposite;
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

    public NavWidget(NavWidget parentNav, final Element heading) {
      super(heading.getInnerHTML(), new ScheduledCommand() {
        @Override
        public void execute() {
          int top = getElementTop(heading) - spyOffset;
          if (isBodyScrollWidget()) {
            Window.scrollTo(Document.get().getScrollLeft(), top);
          }
          else {
            scrollWidget.getElement().setScrollTop(top);
          }
        }
      });

      this.parentNav = parentNav;
      this.level = getLevel(heading);
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

  private final List<Element> headings = Lists.newArrayList();
  private final Map<Element, NavWidget> navs = Maps.newHashMap();

  private boolean refreshing = false;
  private HandlerRegistration scrollRegistration;
  private Widget scrollWidget;

  private int spyOffset;

  private Widget headingContainer;
  private String spyName;

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

  public Widget getHeadingContainer() {
    return headingContainer;
  }

  public void setHeadingContainer(Widget target) {
    this.headingContainer = target;
  }

  public String getSpyName() {
    return spyName;
  }

  public void setSpyName(String spyName) {
    this.spyName = spyName;
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
  public void redraw() {
    if (headingContainer == null) {
      headingContainer = RootPanel.get();
    }
    headings.clear();
    collectHeadings(headingContainer.getElement(), headings);

    int lowestNavLevel = 6;
    navs.clear();
    content.clear();
    for (Element heading : headings) {
      int level = getLevel(heading);
      if (level < lowestNavLevel) {
        lowestNavLevel = level;
      }
    }

    NavWidget currentNav = new NavWidget(null, lowestNavLevel - 1);
    content.add(currentNav.getSubNavContainer());
    for (Element heading : headings) {
      int level = getLevel(heading);
      while (currentNav.level >= level && currentNav.parentNav != null) {
        currentNav = currentNav.parentNav;
      }
      if (currentNav.level < level - 1) {
        for (int i = currentNav.level; i < level; i++) {
          NavWidget newNav = new NavWidget(currentNav, i);
          currentNav.append(newNav);
          currentNav = newNav;
        }
      }

      currentNav = currentNav.addToSubNav(new NavWidget(currentNav, heading));
      navs.put(heading, currentNav);
    }
  }

  private void registerScrollHandler() {
    if (scrollRegistration != null) {
      scrollRegistration.removeHandler();
    }
    if (isBodyScrollWidget()) {
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

  private int getElementTop(Element heading) {
    if (isBodyScrollWidget()) {
      return heading.getAbsoluteTop();
    }
    return heading.getOffsetTop() - scrollWidget.getElement().getOffsetTop();
  }

  private boolean isBodyScrollWidget() {
    return scrollWidget == null;
  }

  private void refreshActive() {
    if (navs.isEmpty()) {
      return; // Not displayed NavSpy
    }
    int scrollTop, scrollHeight, maxScroll;

    if (isBodyScrollWidget()) {
      scrollTop = Document.get().getScrollTop() + spyOffset;
      scrollHeight = Document.get().getScrollHeight();
      maxScroll = scrollHeight - Document.get().getClientHeight();
    }
    else {
      scrollTop = scrollWidget.getElement().getScrollTop() + spyOffset;
      scrollHeight = scrollWidget.getElement().getScrollHeight();
      maxScroll = scrollHeight - scrollWidget.getElement().getClientHeight();
    }

    Element activeHeading = null;
    if (scrollTop >= maxScroll && headings.size() > 0) {
      activeHeading = headings.get(headings.size() - 1);
    }
    else {
      for (Element heading : headings) {
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

  private int getLevel(Element element) {
    return Heading.HEADING_TAGS.indexOf(element.getTagName().toLowerCase()) + 1;
  }

  private void collectHeadings(Element element, List<Element> headings) {
    for (int i = 0; i < element.getChildCount(); i++) {
      Node node = element.getChild(i);
      if (node instanceof Element) {
        Element child = (Element) node;
        String tagName = child.getTagName();
        if (tagName != null && Heading.HEADING_TAGS.contains(tagName.toLowerCase())) {
          if (spyName != null && spyName.equals(child.getAttribute(Heading.ATTRIBUTE_DATA_SUMMARY))) {
            headings.add(child);
          }
        }
        else {
          collectHeadings(child, headings);
        }
      }
    }

  }

}
