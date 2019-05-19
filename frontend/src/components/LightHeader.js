import React, {Component} from "react";
import {Link} from "react-router-dom";
import {Menu} from "semantic-ui-react";

export default class Header extends Component {
    state = {};

    handleItemClick = (e, {name}) => this.setState({activeItem: name});

    render() {
        const {activeItem} = this.state;

        return (
            <Menu>
                <Link to="/">
                    <Menu.Item
                        name="Home"
                        active={activeItem === "Home"}
                        onClick={this.handleItemClick}
                    >
                    </Menu.Item>
                </Link>

                <Link to="/Product">
                    <Menu.Item
                        name="product"
                        active={activeItem === "product"}
                        onClick={this.handleItemClick}
                    >
                        Product
                    </Menu.Item>
                </Link>

                <Menu.Item
                    name="aboutUs"
                    active={activeItem === "aboutUs"}
                    onClick={this.handleItemClick}
                >
                    About Us
                </Menu.Item>

                {/*<Menu.Menu position="right">
                    <Menu.Item
                        name="signUp"
                        active={activeItem === "signUp"}
                        onClick={this.handleItemClick}
                    >
                        <Link to="/SignUp">
                            <Button primary>
                                Sign Up
                            </Button>
                        </Link>
                    </Menu.Item>

                    <Menu.Item
                        name="logIn"
                        active={activeItem === "logIn"}
                        onClick={this.handleItemClick}
                    >
                        <Button>
                            <Link to="/Login">
                                Log In
                            </Link>
                        </Button>
                    </Menu.Item>
                </Menu.Menu>*/}
            </Menu>
        );
    }
}