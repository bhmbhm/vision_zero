import PropTypes from 'prop-types';
import React, {Component} from 'react';
import {Link} from "react-router-dom";
import {
    Button,
    Container,
    Grid,
    Header,
    Icon,
    Image,
    List,
    Menu,
    Responsive,
    Segment,
    Sidebar,
    Visibility,
} from 'semantic-ui-react'
import upturnedCar from '../images/upturnedCar.jpeg';

// Heads up!
// We using React Static to prerender our docs with server side rendering, this is a quite simple solution.
// For more advanced usage please check Responsive docs under the "Usage" section.

// Get width of screen
const getWidth = () => {
    const isSSR = typeof window === 'undefined';

    return isSSR ? Responsive.onlyTablet.minWidth : window.innerWidth
};

/* eslint-disable react/no-multi-comp */
/* Heads up! HomepageHeading uses inline styling, however it's not the best practice. Use CSS or styled components for
 * such things.
 */

const HomepageHeading = ({mobile}) => (
    <Container text>
        <Header
            as='h1'
            content='Vision Zero'
            inverted
            style={{
                fontSize: mobile ? '2em' : '4em',
                fontWeight: 'normal',
                marginBottom: 0,
                marginTop: mobile ? '1.5em' : '3em',
            }}
        />
        <Header
            as='h2'
            content='We Can End Traffic Fatalities in Texas'
            inverted
            style={{
                fontSize: mobile ? '1.5em' : '1.7em',
                fontWeight: 'normal',
                marginTop: mobile ? '0.5em' : '1.5em',
            }}
        />
        {/*<Button primary size='huge'>
            Get Started
            <Icon name='right arrow' />
        </Button>*/}
    </Container>
);

HomepageHeading.propTypes = {
    mobile: PropTypes.bool,
};

/* Heads up!
 * Neither Semantic UI nor Semantic UI React offer a responsive navbar, however, it can be implemented easily.
 * It can be more complicated, but you can create really flexible markup.
 */
class DesktopContainer extends Component {
    state = {};

    hideFixedMenu = () => this.setState({fixed: false});
    showFixedMenu = () => this.setState({fixed: true});

    render() {
        const {children} = this.props;
        const {fixed} = this.state;

        return (
            <Responsive getWidth={getWidth} minWidth={Responsive.onlyTablet.minWidth}>
                <Visibility
                    once={false}
                    onBottomPassed={this.showFixedMenu}
                    onBottomPassedReverse={this.hideFixedMenu}
                >
                    <Segment
                        inverted
                        textAlign='center'
                        style={{minHeight: 700, padding: '1em 0em'}}
                        vertical
                    >
                        <Menu
                            fixed={fixed ? 'top' : null}
                            inverted={!fixed}
                            pointing={!fixed}
                            secondary={!fixed}
                            size='large'
                        >
                            <Container>
                                <Menu.Item as='a' active>
                                    <Link to="/">
                                        Home
                                    </Link>
                                </Menu.Item>
                                <Menu.Item as='a'>
                                    <Link to="/Product">
                                        Product
                                    </Link>
                                </Menu.Item>
                                <Menu.Item as='a'>About Us</Menu.Item>
                                {/*<Menu.Item position='right'>
                                    <Button as='a' inverted={!fixed}>
                                        Log in
                                    </Button>
                                    <Button as='a' inverted={!fixed} primary={fixed} style={{ marginLeft: '0.5em' }}>
                                        Sign Up
                                    </Button>
                                </Menu.Item>*/}
                            </Container>
                        </Menu>
                        <HomepageHeading/>
                    </Segment>
                </Visibility>

                {children}
            </Responsive>
        )
    }
}

DesktopContainer.propTypes = {
    children: PropTypes.node,
};

class MobileContainer extends Component {
    state = {};

    handleSidebarHide = () => this.setState({sidebarOpened: false});

    handleToggle = () => this.setState({sidebarOpened: true});

    render() {
        const {children} = this.props;
        const {sidebarOpened} = this.state;

        return (
            <Responsive
                as={Sidebar.Pushable}
                getWidth={getWidth}
                maxWidth={Responsive.onlyMobile.maxWidth}
            >
                <Sidebar
                    as={Menu}
                    animation='push'
                    inverted
                    onHide={this.handleSidebarHide}
                    vertical
                    visible={sidebarOpened}
                >
                    <Menu.Item as='a' active>
                        Home
                    </Menu.Item>
                    <Menu.Item as='a'>Work</Menu.Item>
                    <Menu.Item as='a'>Company</Menu.Item>
                    <Menu.Item as='a'>Careers</Menu.Item>
                    <Menu.Item as='a'>Log in</Menu.Item>
                    <Menu.Item as='a'>Sign Up</Menu.Item>
                </Sidebar>

                <Sidebar.Pusher dimmed={sidebarOpened}>
                    <Segment
                        inverted
                        textAlign='center'
                        style={{minHeight: 350, padding: '1em 0em'}}
                        vertical
                    >
                        <Container>
                            <Menu inverted pointing secondary size='large'>
                                <Menu.Item onClick={this.handleToggle}>
                                    <Icon name='sidebar'/>
                                </Menu.Item>
                                <Menu.Item position='right'>
                                    <Button as='a' inverted>
                                        Log in
                                    </Button>
                                    <Button as='a' inverted style={{marginLeft: '0.5em'}}>
                                        Sign Up
                                    </Button>
                                </Menu.Item>
                            </Menu>
                        </Container>
                        <HomepageHeading mobile/>
                    </Segment>

                    {children}
                </Sidebar.Pusher>
            </Responsive>
        )
    }
}

MobileContainer.propTypes = {
    children: PropTypes.node,
};

const ResponsiveContainer = ({children}) => (
    <div>
        <DesktopContainer>{children}</DesktopContainer>
        <MobileContainer>{children}</MobileContainer>
    </div>
);

ResponsiveContainer.propTypes = {
    children: PropTypes.node,
};

const HomepageLayout = () => (
    <ResponsiveContainer>
        <Segment style={{padding: '8em 0em'}} vertical>
            <Grid container stackable verticalAlign='middle'>
                <Grid.Row>
                    <Grid.Column width={8}>
                        <Header as='h3' style={{fontSize: '2em'}}>
                            Vision Zero Texas
                        </Header>
                        <p style={{fontSize: '1.33em'}}>
                            <p>Is a project of Farm&City, a 501(c)(3) think and do tank dedicated to high quality urban
                                and rural human habitat in Texas in perpetuity.</p>

                            <p>We don’t want ten people to die every day in the Texas transportation system, and we
                                share this concern with a growing number of Texans.</p>

                            <p>Safe speed policy, comprehensive safety measures, and structural changes to how we
                                approach transportation planning and funding is on the table, if we seize this
                                opportunity.</p>

                            <p>Solving this crisis is complex and requires a thoughtful openness followed by action from
                                governments and communities across the state.</p>
                        </p>
                        <Header as='h3' style={{fontSize: '2em'}}>
                            World Day of Remembrance
                        </Header>
                        <p style={{fontSize: '1.33em'}}>
                            <p>Is an international event held each year to honor road violence victims. An event has
                                been held in Austin for the last several years at Austin City Hall.</p>
                            <p>Several organizations, including Farm&City, Vision Zero ATX, WalkAustin, Central Texas
                                Families for Safe Streets, and BikeAustin have collaborated to host these events and
                                develop and fulfill a policy change each year to honor the victims with actions to
                                prevent future deaths and serious injuries.</p>
                            <p>This year, we are moving the event from the local level to the state, as the City of
                                Austin cannot do this on its own. We need responsible leadership across the state to end
                                traffic deaths.</p>
                        </p>
                    </Grid.Column>
                    <Grid.Column floated='right' width={8}>
                        <Image bordered rounded size='massive' src={upturnedCar}/>
                    </Grid.Column>
                </Grid.Row>
                <Grid.Row>
                    <Grid.Column textAlign='center'>
                        <Link to="/Product">
                        <Button size='huge' color='black'>Our Product<Icon name='right arrow'/></Button>
                        </Link>
                    </Grid.Column>
                </Grid.Row>
            </Grid>
        </Segment>

        {/*<Segment style={{ padding: '0em' }} vertical>
            <Grid celled='internally' columns='equal' stackable>
                <Grid.Row textAlign='center'>
                    <Grid.Column style={{ paddingBottom: '5em', paddingTop: '5em' }}>
                        <Header as='h3' style={{ fontSize: '2em' }}>
                            "What a Company"
                        </Header>
                        <p style={{ fontSize: '1.33em' }}>That is what they all say about us</p>
                    </Grid.Column>
                    <Grid.Column style={{ paddingBottom: '5em', paddingTop: '5em' }}>
                        <Header as='h3' style={{ fontSize: '2em' }}>
                            "I shouldn't have gone with their competitor."
                        </Header>
                        <p style={{ fontSize: '1.33em' }}>
                            <Image avatar src='/images/avatar/large/nan.jpg' />
                            <b>Nan</b> Chief Fun Officer Acme Toys
                        </p>
                    </Grid.Column>
                </Grid.Row>
            </Grid>
        </Segment>*/}

        <Segment style={{padding: '3em 0em'}} vertical>
            <Container text>
                {/*<Header as='h3' style={{ fontSize: '2em' }}>
                    Breaking The Grid, Grabs Your Attention
                </Header>
                <p style={{ fontSize: '1.33em' }}>
                    Instead of focusing on content creation and hard work, we have learned how to master the
                    art of doing nothing by providing massive amounts of whitespace and generic content that
                    can seem massive, monolithic and worth your attention.
                </p>
                <Button as='a' size='large'>
                    Read More
                </Button>*/}

                {/*<Divider
                    as='h3'
                    className='header'
                    horizontal
                    style={{ margin: '3em 0em', textTransform: 'uppercase' }}
                >
                    <a href='#'>Our Statistics</a>
                </Divider>*/}

                <Header as='h3' style={{fontSize: '2em'}}>
                    Our Statistics
                </Header>
                <Grid columns={3} divided>
                    <Grid.Row>
                        <Grid.Column>
                            <Image src={upturnedCar}/>
                            237 were fatal accidents, claiming 245 lives
                        </Grid.Column>
                        <Grid.Column>
                            <Image src={upturnedCar}/>
                            5,397 were classified as injury accidents, leaving 7,326 people injured
                        </Grid.Column>
                        <Grid.Column>
                            <Image src={upturnedCar}/>
                            43,027 were non-injury accidents
                        </Grid.Column>
                    </Grid.Row>

                    <Grid.Row>
                        <Grid.Column>
                            <Image src={upturnedCar}/>
                            1,118 were serious injury accidents, leaving 1,304 people seriously injured
                        </Grid.Column>
                        <Grid.Column>
                            <Image src={upturnedCar}/>
                            23,385 accidents with possible injuries
                        </Grid.Column>
                        <Grid.Column>
                            <Image src={upturnedCar}/>
                            2,630 were classified as "unknown severity"
                        </Grid.Column>
                    </Grid.Row>
                </Grid>
                {/*<Button as='a' size='large'>
                    I'm Still Quite Interested
                </Button>*/}
            </Container>
        </Segment>

        <Segment inverted vertical style={{padding: '5em 0em'}}>
            <Container>
                <Grid divided inverted stackable>
                    <Grid.Row>
                        <Grid.Column width={3}>
                            <Header inverted as='h4' content='About'/>
                            <List link inverted>
                                <List.Item as='a'>Sitemap</List.Item>
                                <List.Item as='a'>Contact Us</List.Item>
                                <List.Item as='a'>Product</List.Item>
                                <List.Item as='a'>About Us</List.Item>
                            </List>
                        </Grid.Column>
                        <Grid.Column width={3}>
                            <Header inverted as='h4' content='Services'/>
                            <List link inverted>
                                <List.Item as='a'>FAQ</List.Item>
                                <List.Item as='a'>Statistics</List.Item>
                                <List.Item as='a'>Our Vision</List.Item>
                                <List.Item as='a'>Our Mission</List.Item>
                            </List>
                        </Grid.Column>
                        <Grid.Column width={7}>
                            <Header as='h4' inverted>
                                We can end traffic deaths in Texas
                            </Header>
                            <p>
                                But we need leadership from the Texas Legislature and Governor. Vision Zero Texas is
                                collaborating with communities and organizations from across the state to support this
                                legislative agenda for #txlege 2019.
                            </p>
                        </Grid.Column>
                    </Grid.Row>
                </Grid>
            </Container>
        </Segment>
    </ResponsiveContainer>
);

export default HomepageLayout
